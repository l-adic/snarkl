use clap::{App, Arg};
use num_bigint::BigUint;
use serde::{Deserialize, Deserializer, Serialize};
use std::fs::File;
use std::io::{self, BufRead};
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
struct Header {
    extension_degree: i32,
    #[serde(deserialize_with = "deserialize_biguint")]
    field_characteristic: BigUint,
    input_variables: Vec<i32>,
    n_constraints: i32,
    n_variables: i32,
    output_variables: Vec<i32>,
}

// Custom function to deserialize a Vec<(BigUint, u64)> from an array of arrays
fn deserialize_biguint_tuple_vec<'de, D>(deserializer: D) -> Result<Vec<(BigUint, u64)>, D::Error>
where
    D: Deserializer<'de>,
{
    // Expecting a vector of arrays, where each array is [String, u64]
    let v: Vec<(String, u64)> = Deserialize::deserialize(deserializer)?;
    v.into_iter()
        .map(|(biguint_str, uint)| {
            let biguint = BigUint::from_str(&biguint_str).map_err(serde::de::Error::custom)?;
            Ok((biguint, uint))
        })
        .collect()
}

#[derive(Serialize, Deserialize, Debug)]
struct Entry {
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec")]
    A: Vec<(BigUint, u64)>,
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec")]
    B: Vec<(BigUint, u64)>,
    #[serde(deserialize_with = "deserialize_biguint_tuple_vec")]
    C: Vec<(BigUint, u64)>,
}
fn main() -> io::Result<()> {
    // Use clap to handle command line arguments
    let matches = App::new("JSONL Parser")
        .version("1.0")
        .author("Your Name")
        .about("Parses JSONL files")
        .arg(
            Arg::with_name("file")
                .help("The JSONL file to parse")
                .required(true)
                .index(1),
        )
        .get_matches();

    // Get the file name from the command line argument
    let file_name = matches.value_of("file").unwrap();

    // Open the file in read-only mode
    let file = File::open(file_name)?;
    let reader = io::BufReader::new(file);

    for (index, line) in reader.lines().enumerate() {
        let line = line?;

        // Parse the first line as the header, and others as entries
        if index == 0 {
            let header: Header = serde_json::from_str(&line).expect("Error parsing header");
            println!("Header: {:?}", header);
        } else {
            let entry: Entry = serde_json::from_str(&line).expect("Error parsing entry");
            println!("Entry {}: {:?}", index, entry);
        }
    }

    Ok(())
}
