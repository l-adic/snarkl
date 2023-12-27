use ark_bn254::Bn254;
use ark_ec::pairing::Pairing;
use ark_relations::r1cs::{
    ConstraintSynthesizer, ConstraintSystemRef, LinearCombination, SynthesisError, Variable,
};
use clap::{App, Arg};
use num_bigint::BigUint;
use serde::de::IntoDeserializer;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{self, Value};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::str::FromStr; // Import IntoDeserializer trait

// Custom function to deserialize BigUint from a string
fn deserialize_biguint<'de, D>(deserializer: D) -> Result<BigUint, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    BigUint::from_str(&s).map_err(serde::de::Error::custom)
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Header {
    extension_degree: usize,
    #[serde(deserialize_with = "deserialize_biguint")]
    field_characteristic: BigUint,
    input_variables: HashSet<usize>,
    n_constraints: usize,
    n_variables: usize,
    output_variables: Vec<usize>,
}

fn deserialize_coeff_var_tuple<'de, D, E>(
    deserializer: D,
) -> Result<(usize, E::ScalarField), D::Error>
where
    D: Deserializer<'de>,
    E: Pairing,
    E::ScalarField: FromStr,
{
    let (var, coeff): (usize, String) = Deserialize::deserialize(deserializer)?;
    E::ScalarField::from_str(&coeff)
        .map(|field_element| (var, field_element))
        .map_err(|_| serde::de::Error::custom("Error in ScalarField parser"))
    // Use Debug formatting
}

fn deserialize_biguint_tuple_vec<'de, D, E>(
    deserializer: D,
) -> Result<Vec<(E::ScalarField, usize)>, D::Error>
where
    D: Deserializer<'de>,
    E: Pairing,
    E::ScalarField: FromStr,
{
    let vec: Vec<(String, usize)> = Deserialize::deserialize(deserializer)?;
    vec.into_iter()
        .map(|(coeff, var)| {
            E::ScalarField::from_str(&coeff)
                .map(|field_element| (field_element, var))
                .map_err(|_| serde::de::Error::custom("Error in ScalarField parser"))
        })
        .collect()
}

#[derive(Deserialize, Debug, Clone)]
pub struct R1C<E: Pairing> {
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec::<_, E>")]
    A: Vec<(E::ScalarField, usize)>,
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec::<_, E>")]
    B: Vec<(E::ScalarField, usize)>,
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec::<_, E>")]
    C: Vec<(E::ScalarField, usize)>,
}

pub struct R1CSFile<E: Pairing> {
    pub header: Header,
    pub constraints: Vec<R1C<E>>,
}

#[derive(Clone, Debug)]
pub struct R1CS<E: Pairing> {
    pub inputs_variables: HashSet<usize>,
    pub witness_variables: HashSet<usize>,
    pub constraints: Vec<R1C<E>>,
}

impl<E: Pairing> From<R1CSFile<E>> for R1CS<E> {
    fn from(file: R1CSFile<E>) -> Self {
        let var_set = (0..file.header.n_variables).collect::<HashSet<usize>>();
        let input_vars = file.header.input_variables.clone(); // Clone the input_variables

        R1CS {
            inputs_variables: file.header.input_variables,
            witness_variables: var_set
                .iter()
                .copied() // Add the copied method here
                .filter(|i| !input_vars.contains(i))
                .collect(),
            constraints: file.constraints,
        }
    }
}

impl<E: Pairing> ConstraintSynthesizer<E::ScalarField> for R1CS<E> {
    fn generate_constraints(
        self: Self,
        cs: ConstraintSystemRef<E::ScalarField>,
    ) -> Result<(), SynthesisError> {
        // Start from 1 because Arkworks implicitly allocates One for the first input
        for x in self.inputs_variables {
            cs.new_input_variable(|| Ok(E::ScalarField::from(1u32)))?;
        }

        for x in self.witness_variables {
            cs.new_witness_variable(|| Ok(E::ScalarField::from(1u32)))?;
        }

        let make_lc = |lc_data: &[(E::ScalarField, usize)]| {
            lc_data.iter().fold(
                LinearCombination::<E::ScalarField>::zero(),
                |lc: LinearCombination<E::ScalarField>, (coeff, index)| {
                    lc + (*coeff, Variable::Instance(*index))
                },
            )
        };

        for constraint in &self.constraints {
            cs.enforce_constraint(
                make_lc(&constraint.A),
                make_lc(&constraint.B),
                make_lc(&constraint.C),
            )?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct WitnessFile<E: Pairing> {
    pub header: Header,
    pub witness: Vec<(usize, E::ScalarField)>,
}

pub struct Witness<E: Pairing> {
    pub input_variables: HashMap<usize, E::ScalarField>,
    pub aux_variables: HashMap<usize, E::ScalarField>,
}

impl<E: Pairing> Witness<E> {
    pub fn new(file: WitnessFile<E>) -> Self {
        let mut input_variables: HashMap<usize, E::ScalarField> = HashMap::new();
        let mut aux_variables: HashMap<usize, E::ScalarField> = HashMap::new();

        file.witness.into_iter().for_each(|(index, value)| {
            if file.header.input_variables.contains(&index) {
                input_variables.insert(index, value);
            } else {
                aux_variables.insert(index, value);
            }
        });

        Witness {
            input_variables,
            aux_variables,
        }
    }
}

fn main() -> io::Result<()> {
    // Clap to handle command line arguments
    let matches = App::new("straw-arkworks-bridge")
        .version("1.0")
        .arg(
            Arg::with_name("file")
                .help("The R1CS JSONL file to parse")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("witness")
                .help("Optional witness JSONL file")
                .required(false)
                .index(2),
        )
        .get_matches();

    // Get file name from command line argument
    let file_name = matches.value_of("file").unwrap();

    // Open file in read-only mode
    let file = File::open(file_name)?;
    let reader = BufReader::new(file);

    let mut lines = reader.lines();

    // Read and parse header line
    let header_line = lines.next().ok_or(io::Error::new(
        io::ErrorKind::NotFound,
        "Header line not found",
    ))??;
    let header: Header = serde_json::from_str(&header_line).expect("Error parsing header");

    // Read and parse constraints
    let constraints: Vec<R1C<Bn254>> = lines
        .map(|line| {
            let line = line.expect("Error reading line");
            serde_json::from_str(&line).expect("Error parsing constraint")
        })
        .collect();

    // Create R1CSFile and convert it into R1CS
    let r1cs_file = R1CSFile {
        header,
        constraints,
    };

    let r1cs: R1CS<Bn254> = r1cs_file.into();

    println!("R1CS: {:?}", r1cs);

    // generate witness file

    //let setup = Groth16::<Bn254>::circuit_specific_setup(r1cs, &mut thread_rng()).unwrap();
    //println!("Setup: {:?}", setup);
    // ::<Bn254, _, _>(r1cs, &mut thread_rng()).unwrap();

    if let Some(witness_file_name) = matches.value_of("witness") {
        let witness_file = File::open(witness_file_name)?;
        let mut witness_reader = BufReader::new(witness_file);

        let mut lines = witness_reader.lines();

        // Read and parse witness header line
        let header_line = lines.next().ok_or(io::Error::new(
            io::ErrorKind::NotFound,
            "Witness header line not found",
        ))??;
        let witness_header: Header =
            serde_json::from_str(&header_line).expect("Error parsing witness header");

        println!("Witness header: {:?}", witness_header);

        let mut witness_data = Vec::new();
        for line in lines {
            let line = line.expect("Error reading line from witness file");
            let json = serde_json::from_str::<Value>(&line).expect("Error parsing JSON to Value");
            let deserializer = json.into_deserializer();
            let parsed_data = deserialize_coeff_var_tuple::<_, Bn254>(deserializer)
                .expect("Error in custom deserialization");
            witness_data.push(parsed_data);
        }

        // Create WitnessFile
        let witness_file: WitnessFile<Bn254> = WitnessFile {
            header: witness_header,
            witness: witness_data,
        };

        println!("Witness file: {:?}", witness_file);

        // Use the witness file as needed
        // For example, you might integrate it into your R1CS processing
    }

    // Rest of your code

    Ok(())
}
