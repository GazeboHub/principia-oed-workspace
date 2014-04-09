
Principia Open EDA Desk - Project Workspace
===============================

## Metadata

Project name: `principia-oed-workspace`

Availability: <https://github.com/GazeboHub/principia-oed-workspace>

## Components


### gUtils

Pathname: `src/main/labview-g/g-utils`
Latest version: 1.0.2

#### gUtils - Overview

This afternoon, I've created a small set of National Instruments (NI)
LabVIEW Virtual Instruments (VI), such that can be used for performing
calculations in series and parallel circuit analysis. I've attached a
LabVIEW project to this forum item, a zip file containing the LabVIEW
lvproj files. This is version 1.0.2 of an EELaw project I've begun, in
the interest of developing a library of software tools for
facilitating manual circuit analysis.  The VIs contained of the
project (a/o 1.0.2) are as follows:

* `RpR.vi` - application: calculate the equivalent resistance of two
  passive circuit elements in parallel

* `Div-Cur.VI` - application: calculate current at individual parallel
resistors (current divider) for two parallel circuit branches

* `Div-Volt.vi` - application: calculate voltage differential across
individual series resistors (voltage divider) for two series resistors

#### gUtils - Caveats (1.0.2)

I've not yet tested Div-Cur and Div-Volt thoroughly. I've tested that
those VIs will calculate values, given "10" across the inputs, but  in
the "Div-" VIs, I have not yet checked to verify the algorithms. I
understand that I could verify the algorithms, using parallel and
series circuits in Multisim (also over Citrix). Of course, I would
want to contain the test-bench circuits along with the project files
themselves, then, in 1.0.3

The VIs in 1.0.2 do not preform unit analysis. It's assumed that all
inputs are in basic SI units -- ohms, amperes, volts.

The VIs each contain some documentation in the VI's "Documentation"
field, describing the intended application and the algorithm
implemented in each. I  would like to write some further documentation
for those items, to better explain their applications for later
reference, though frankly I'm not certain of what direction the
project may proceed in from there.

#### gUtils - "Blue Sky"

It may be possible to design a _finite state machine_, such that may be
used to _automate_ and to _verify_ a process of _circuit analysis_,
efficiently, via a procedure logically equivalent to the procedures
of reduction and calculation in  manual circuit analysis -- of
reducing series and parallel resistances into equivalent resistances,
in an iterative process.

There may be a more efficient approach than that, used for circuit
voltage, current, and resistance analysis in regular schematic design
programs. Though it might not be highly efficient, in its iterative
two-resistor model, however it could make for a useful exercise in
program design and circuit modeling.
