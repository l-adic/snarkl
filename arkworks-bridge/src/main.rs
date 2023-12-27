mod circuit;
mod header;
mod r1cs;
mod witness;

use crate::circuit::Circuit;
use crate::witness::Witness; // Import IntoDeserializer trait
use ark_bn254::Bn254;
use ark_crypto_primitives::snark::*;
use ark_groth16::Groth16;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use clap::{App, Arg};
use r1cs::{parse_r1cs_file, R1CS};
use rand::thread_rng;
use std::fs::File;
use std::io::{self, BufReader};
use std::path::PathBuf;
use structopt::StructOpt;
use witness::{parse_witness_file, WitnessFile};

#[derive(StructOpt, Debug)]
#[structopt(name = "straw-arkworks-bridge")]
struct Cli {
    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt, Debug)]
enum Command {
    CreateTrustedSetup {
        #[structopt(short, long, parse(from_os_str))]
        r1cs_path: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        pk_output: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        vk_output: PathBuf,
    },
    /// Read the proving key, witness file, and R1CS file to create a proof
    CreateProof {
        #[structopt(short, long, parse(from_os_str))]
        proving_key: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        witness: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        r1cs: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        output: PathBuf,
    },

    /// Read the verifying key, proof, and witness file to verify the proof
    VerifyProof {
        #[structopt(short, long, parse(from_os_str))]
        verifying_key: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        proof: PathBuf,

        #[structopt(short, long, parse(from_os_str))]
        inputs: PathBuf,
    },
}

fn create_trusted_setup(
    r1cs_path: PathBuf,
    pk_output: PathBuf,
    vk_output: PathBuf,
) -> io::Result<()> {
    let file = File::open(r1cs_path)?;
    let reader = BufReader::new(file);

    let r1cs: R1CS<Bn254> = parse_r1cs_file(reader)?.into();

    let circuit = Circuit {
        r1cs,
        witness: None,
    };

    let setup = Groth16::<Bn254>::circuit_specific_setup(circuit, &mut thread_rng()).unwrap();

    // Serialize the proving key to the output file
    let mut file = File::create(pk_output)?;
    setup.0.serialize_uncompressed(&mut file).map_err(|e| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("Failed to serialize proving key: {}", e),
        )
    })?;

    // Serialize the verifying key to the output file
    let mut file = File::create(vk_output)?;
    setup.1.serialize_uncompressed(&mut file).map_err(|e| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("Failed to serialize verifying key: {}", e),
        )
    })
}

fn create_proof(
    proving_key: PathBuf,
    witness: PathBuf,
    r1cs: PathBuf,
    output: PathBuf,
) -> io::Result<()> {
    let file = File::open(proving_key)?;
    let mut reader = BufReader::new(file);

    let proving_key =
        <Groth16<Bn254> as ark_crypto_primitives::snark::SNARK<ark_bn254::Fr>>::ProvingKey::deserialize_uncompressed(&mut reader).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Failed to deserialize proving key: {}", e),
            )
        })?;

    let file = File::open(witness)?;
    let reader = BufReader::new(file);

    let witness_file: WitnessFile<Bn254> = parse_witness_file(reader)?;

    let witness = Witness::new(witness_file);

    let file = File::open(r1cs)?;
    let reader = BufReader::new(file);

    let r1cs: R1CS<Bn254> = parse_r1cs_file(reader)?.into();

    let circuit = Circuit {
        r1cs,
        witness: Some(witness),
    };

    let proof = Groth16::<Bn254>::prove(&proving_key, circuit, &mut thread_rng()).unwrap();

    // Serialize the proof to the output file
    let mut file = File::create(output)?;
    proof.serialize_uncompressed(&mut file).map_err(|e| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("Failed to serialize proof: {}", e),
        )
    })
}

fn verify_proof(verifying_key: PathBuf, proof: PathBuf, inputs: PathBuf) -> io::Result<()> {
    let file = File::open(verifying_key)?;
    let mut reader = BufReader::new(file);

    let verifying_key =
        <Groth16<Bn254> as ark_crypto_primitives::snark::SNARK<ark_bn254::Fr>>::VerifyingKey::deserialize_uncompressed(&mut reader).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Failed to deserialize verifying key: {}", e),
            )
        })?;

    let file = File::open(proof)?;
    let mut reader = BufReader::new(file);

    let proof =
        <Groth16<Bn254> as ark_crypto_primitives::snark::SNARK<ark_bn254::Fr>>::Proof::deserialize_uncompressed(&mut reader).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Failed to deserialize proof: {}", e),
            )
        })?;

    let file = File::open(inputs)?;
    let reader = BufReader::new(file);

    let witness_file: WitnessFile<Bn254> = parse_witness_file(reader)?;
    let witness = Witness::new(witness_file);
    let mut input_tuples: Vec<(usize, ark_bn254::Fr)> =
        witness.input_variables.into_iter().collect();
    input_tuples.sort_by(|(a, _), (b, _)| a.cmp(b));
    let inputs: Vec<ark_bn254::Fr> = input_tuples.into_iter().map(|(_, v)| v).collect();

    let pvk = Groth16::<Bn254>::process_vk(&verifying_key).unwrap();

    let result = Groth16::<Bn254>::verify_with_processed_vk(&pvk, &inputs, &proof).unwrap();

    println!("Verification result: {}", result);

    Ok(())
}

fn main() -> io::Result<()> {
    // Clap to handle command line arguments

    let args = Cli::from_args();

    match args.command {
        Command::CreateTrustedSetup {
            r1cs_path,
            pk_output,
            vk_output,
        } => {
            create_trusted_setup(r1cs_path, pk_output, vk_output)?;
        }
        Command::CreateProof {
            proving_key,
            witness,
            r1cs,
            output,
        } => {
            create_proof(proving_key, witness, r1cs, output)?;
        }
        Command::VerifyProof {
            verifying_key,
            proof,
            inputs,
        } => {
            verify_proof(verifying_key, proof, inputs)?;
        }
    }

    Ok(())
}
