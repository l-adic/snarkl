mod circuit;
mod header;
mod r1cs;
mod witness;

use ark_bn254::Bn254;
use ark_crypto_primitives::snark::*;
use ark_ec::pairing::Pairing;
use ark_groth16::Groth16;
use ark_relations::r1cs::{
    ConstraintSynthesizer, ConstraintSystemRef, LinearCombination, SynthesisError, Variable,
};
use clap::{App, Arg};
use r1cs::{parse_r1cs_file, R1CS};
use rand::thread_rng;
use std::fs::File;
use std::io::{self, BufReader};
use witness::{parse_witness_file, WitnessFile};

use crate::circuit::Circuit;
use crate::witness::Witness; // Import IntoDeserializer trait

impl<E: Pairing> ConstraintSynthesizer<E::ScalarField> for R1CS<E> {
    fn generate_constraints(
        self: Self,
        cs: ConstraintSystemRef<E::ScalarField>,
    ) -> Result<(), SynthesisError> {
        // Start from 1 because Arkworks implicitly allocates One for the first input
        for _ in self.inputs_variables {
            cs.new_input_variable(|| Ok(E::ScalarField::from(1u32)))?;
        }

        for _ in self.witness_variables {
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

    let r1cs: R1CS<Bn254> = parse_r1cs_file(reader)?.into();

    println!("R1CS: {:?}", r1cs);

    if let Some(witness_file_name) = matches.value_of("witness") {
        let witness_file = File::open(witness_file_name)?;
        let witness_reader = BufReader::new(witness_file);

        let witness_file: WitnessFile<Bn254> = parse_witness_file(witness_reader)?;

        let witness = Witness::new(witness_file);

        println!("Witness file: {:?}", witness);

        let circuit = Circuit { r1cs, witness };

        let setup = Groth16::<Bn254>::circuit_specific_setup(circuit, &mut thread_rng()).unwrap();

        println!("Setup: {:?}", setup);
    }

    // Rest of your code

    Ok(())
}
