
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

This afternoon, I've created a small set of [National Instruments (NI)
LabVIEW(tm)][labview] _Virtual Instruments_ (VI), such that can be
used for performing calculations of _resistance_, _current_, and
_voltage_ quantities in _series_ and _parallel_ _circuit analysis_.

The VIs contained of the gUtils component (version 1.0.2) are as
follows:

* `RpR.vi` - application: calculate the equivalent resistance of two
  passive circuit elements in parallel

* `Div-Cur.VI` - application: calculate current at individual parallel
resistors (current divider) for two parallel circuit branches

* `Div-Volt.vi` **(1.0.2 DO NOT USE)** application: calculate voltage
   differential across individual series resistors (voltage divider)
   for two series resistors

#### gUtils - Caveats (1.0.2)

* The Div- VIs (gUtils 1.0.2) both _re-use_ `RpR.vi` for calculatiig
  the _equivalent resistance (rEQ)_ ... **and there is therefore a bug
  in_Div-Volt.VI_ as it must calculate _resistance in series_, instead.**

* The VIs Div-Cur and Div-Volt have not been tested thoroughly
  (gUtils 1.0.2). Those VIs have been tesetd to calculate values, given
  "10" across the inputs, but in the "Div-" VIs, those have not been
  checked to verify the algorithms.

* The VIs in 1.0.2 do not preform unit analysis. It's assumed that all
  inputs are in basic SI units -- ohms, amperes, volts.

* The VIs each contain some documentation in the VI's "Documentation"
  field, describing the intended application and the algorithm
  implemented in each. Some further documentation may be develoepd
  for those items, to better explain their applications for later
  reference.

#### gUtils - "Blue Sky"

It may be possible to design a _finite state machine_ (FSM) such that
may be used to _automate_ and to _verify_ a process of _circuit
analysis_, efficiently, via a procedure logically equivalent to the
procedures of reduction and calculation in  manual circuit analysis --
peformed "Two resistances at a time," mainly -- of reducing series and
parallel resistances into equivalent resistances, in an iterative
process.

There may be a more efficient approach than that, available for
applications in circuit voltage, current, and resistance
analysis. Though the _binary FSM_ methodology might not seem optimally
efficient, in its iterative two-resistor model, however it could make
for a useful exercise in program design and circuit modeling.


#### gUtils - Version History

* 1.0.1 : created `RpR.vi` and containing `EELaw.lvproj` project
* 1.0.2 : added `Div-Cur.vi` and `Div-Volt.vi` both in reusing `RpR.vi`

[labview]: http://www.ni.com/labview/
