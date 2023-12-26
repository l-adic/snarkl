use ark_bn254::Bn254;
use ark_crypto_primitives::snark::SNARK;
use ark_ec::pairing::Pairing;
use ark_groth16::{self, Groth16};
use ark_relations::r1cs::{
    ConstraintSynthesizer, ConstraintSystemRef, LinearCombination, SynthesisError, Variable,
};
use ark_std::rand::thread_rng;
use clap::{App, Arg};
use num_bigint::BigUint;
use serde::{Deserialize, Deserializer, Serialize};
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::str::FromStr;

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
    extension_degree: i32,
    #[serde(deserialize_with = "deserialize_biguint")]
    field_characteristic: BigUint,
    input_variables: Vec<i32>,
    n_constraints: i32,
    n_variables: i32,
    output_variables: Vec<i32>,
}

fn deserialize_biguint_tuple_vec<'de, D, E>(
    deserializer: D,
) -> Result<Vec<(E::ScalarField, usize)>, D::Error>
where
    D: Deserializer<'de>,
    E: Pairing,
    E::ScalarField: FromStr,
{
    let v: Vec<(String, usize)> = Deserialize::deserialize(deserializer)?;
    v.into_iter()
        .map(|(s, uint)| {
            E::ScalarField::from_str(&s)
                .map(|field_element| (field_element, uint))
                .map_err(|_| serde::de::Error::custom("Error in ScalarField parser"))
            // Use Debug formatting
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
    pub num_inputs: usize,
    pub num_aux: usize,
    pub num_variables: usize,
    pub constraints: Vec<R1C<E>>,
}

impl<E: Pairing> From<R1CSFile<E>> for R1CS<E> {
    fn from(file: R1CSFile<E>) -> Self {
        let num_inputs = file.header.input_variables.len() as usize;
        let num_variables = file.header.n_variables as usize;
        let num_aux = num_variables - num_inputs;
        R1CS {
            num_aux,
            num_inputs,
            num_variables,
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
        for _ in 1..(self.num_inputs + 1) {
            cs.new_input_variable(|| Ok(E::ScalarField::from(1u32)))?;
        }

        for _ in 0..self.num_aux {
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

    let setup = Groth16::<Bn254>::circuit_specific_setup(r1cs, &mut thread_rng()).unwrap();
    println!("Setup: {:?}", setup);
    // ::<Bn254, _, _>(r1cs, &mut thread_rng()).unwrap();

    Ok(())
}
