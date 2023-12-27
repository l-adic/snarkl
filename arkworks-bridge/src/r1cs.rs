use crate::header::Header;
use ark_bn254::Bn254;
use ark_ec::pairing::Pairing;
use serde::{Deserialize, Deserializer};
use std::collections::HashSet;
use std::fmt::Debug;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::str::FromStr; // Import IntoDeserializer trait

fn deserialize_coeff_tuple_vec<'de, D, E>(
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
    #[serde(deserialize_with = "deserialize_coeff_tuple_vec::<_, E>")]
    pub A: Vec<(E::ScalarField, usize)>,
    #[serde(deserialize_with = "deserialize_coeff_tuple_vec::<_, E>")]
    pub B: Vec<(E::ScalarField, usize)>,
    #[serde(deserialize_with = "deserialize_coeff_tuple_vec::<_, E>")]
    pub C: Vec<(E::ScalarField, usize)>,
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

pub fn parse_r1cs_file(reader: BufReader<File>) -> io::Result<R1CSFile<Bn254>> {
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

    Ok(R1CSFile {
        header,
        constraints,
    })
}
