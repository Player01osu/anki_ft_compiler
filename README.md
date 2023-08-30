
# Basic Idea

  Simple transpiler for ankift filetype. Generates `.txt` files (csv style)
  that can be directly imported to anki.

# Syntax

```
> let deck = "1::UChem CH1150::Particles of Matter";
> let notetype = "Basic (and reversed card)";

#[Basic]
Element/Pure Substance; A pure substance that cannot be separated into simpler substances by chemical or physical methods.;

#[]
Compound;
A pure substance containing two or more <b>different</b> elements that cannot be separated by physical methods.;

#[]
Diatomic Molecule; Br, I, N, Cl, H, O, F;

#[]
Chemical Equation;
Substances involved in a chemical reaction and include information about the phases of each substance;

#[]
Filtration;
A process that separates solids and fluids in a mixure using a filter medium that has a structure through which only the fluid can pass.;

#[]
Chemical Reaction;
The transformation of one or more substances into different substances;

#[]
Mixture;
Composed of two or more substances that retain their own chemical identities;

#[]
Homogeneous Mixure;
A mixure in which the components are distributed uniformily throughout.;

#[]
Heterogeneous Mixure;
Not distributed uniformaly;

#[]
Law of Constant Composition;
All samples of a compound contain the same elements combined in the same proportions.;

#[]
Intensive Properties;
Properties that characterize matter independent of the quantity of it present.;

#[]
Extensive Properties;
Properties that depend on how much substance is present.;

#[]
Physical Properties;
Properties that can be observed or measured without changing the substance into another substance.;

#[]
Chemical Properties;
Properties that can only be determined by reacting one substance with another.;

#[Basic]
Physical or chemical: Gold melts at 1064°C.;
Physical;

#[]
Scientific Method;
Approach to acquiring knowledge based on observation of phenomena, development of a testable hypothesis, and additional experiments that test the validity of the hypothesis.;

> let deck = "1::UChem CH1150::Atoms, Ions, and Molecules";

#[]
JJ Thompson's Contribution;
Used a cathode ray tube to discover an electron.

Particles emitted from the CRT exhibited a charge and were affected by magnetic fields.
He concluded that these particles are electrons.

<img src=cathodray-experiment.png>;

#[]
Robert Millikan's Contribution;

Used x-ray and oil drops to determine the charge of particles.

<img src=millikan-oil-drop.png>;
```
