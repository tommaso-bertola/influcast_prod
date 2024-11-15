# Influcast
The current repository hosts the software under development for the modeling of seasonal influenza in Italy.
The main purpose is to produce weekly estimates for the near-future incidence at the national and regional level in Italy of influenza, and more in general influenza-like-illnesses (ILI).

This piece of code is part of a challenge organized by ISI, called Influcast, to which several groups take part providing independent estimates.

## How does it work
Data is provided weekly in a machine readable format.
The data contains the incidence at the regional level and at the national level. 
There is the additional indication if the incidence refers to the overall incidence or that of influenza A and B alone.
In principle a simple generalisation of the dynamical model to fit is enough to fit all these information, but it is not always guaranteed to return good estimates.

What happens under the hood is made clear by first understanding the algorithm called Particle Swarm Optimisation.
Several values of parameters are tested and their performance is controlled by the fitness function.
The swarm updates itself trying to converge towards the global minimum in the parameters phase space. 
When the number of iterations runs out, a solution is returned.
This procedure is repeated several times until some statistics is gathered.
By the obtained results it is possible to gather some meaning on the parameters used (being them initial conditions or the recovery or infecting rates of the pathogens) and predicting the outcomes in the following 4 weeks.

The epidemic model used is a multipathogen compartmental model with the additional effects of mobility mapped in the force of infection.
Please see [for reference](https://hdl.handle.net/20.500.12608/70806).

## Models used as a test
In the following a brief list of several variations in the model definition are reported, in order to keep track of their variations.
The backbone remains the same, but different parameters are sometimes used, thus creating a different model.

Similarly, there are different variations in the way the model itself is calibrated. As such another table reporting the variations in the fitness function is shown.


| Epidemic model | Description | With age groups |
| ---|---|---|
|SIR_multistrain_3|initial conditions depend on age and region. force of infection depends on age-age infectivity|yes|
|SIR_multistrain_4|initial conditions do not depend on age, but force of infections does| yes|
|SIR_multipathogen_5|initial conditions depend on age only, force of infection depends on age. mu depends on age group| yes|
|SIR_multistrain_no_age|initial conditions depend on regions only. simplified force of infection: only one age group. mu does not depend on age|no|
|SIR_multistrain_2|prototypical version. deprecated|NA|
|SEIR|deprecated|NA|
|SIIR|deprecated|NA|