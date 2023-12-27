use crate::{r1cs::R1CS, witness::Witness};
use ark_ec::pairing::Pairing;
use ark_relations::r1cs::{
    ConstraintSynthesizer, ConstraintSystemRef, LinearCombination, SynthesisError, Variable,
}; // Import IntoDeserializer trait

pub struct Circuit<E: Pairing> {
    pub r1cs: R1CS<E>,
    pub witness: Witness<E>,
}

impl<E: Pairing> ConstraintSynthesizer<E::ScalarField> for Circuit<E> {
    fn generate_constraints(
        self: Self,
        cs: ConstraintSystemRef<E::ScalarField>,
    ) -> Result<(), SynthesisError> {
        // Start from 1 because Arkworks implicitly allocates One for the first input
        for _ in self.witness.input_variables {
            cs.new_input_variable(|| Ok(E::ScalarField::from(1u32)))?;
        }

        for _ in self.witness.aux_variables {
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

        for constraint in &self.r1cs.constraints {
            cs.enforce_constraint(
                make_lc(&constraint.A),
                make_lc(&constraint.B),
                make_lc(&constraint.C),
            )?;
        }

        Ok(())
    }
}
