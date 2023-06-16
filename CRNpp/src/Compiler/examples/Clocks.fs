// Author: Roar Nind Steffensen, 16/06/2023

namespace Examples

module Clocks =
    open ChemicalEngine
    open Rendering.Plotting
    open Reaction

    let fakeClock () =
      let A = ("A", 1.0)
      let B = ("B", 0.0)
      let C = ("C", 0.0)
      let species = [A;B;C]
      let formula :Formula= [
          [Normal (CRN.fromReactionAndSpecies [([name A, 1], 1.0, [name A, 1; name B, 1])] species)];
          [Normal (CRN.fromReactionAndSpecies [([name A, 1], 1.0, [name A, 1; name C, 1])] species)];
      ]

      let stepCount = List.length formula
      let duration = 2.0 * float Clock.stepPeriod * FakeClockSimulator.clockPhaseDuration * float stepCount
      let data = formula 
                |> FakeClockSimulator.watchConstRes duration 

      let xs = Seq.initInfinite Simulator.constResTime |> Seq.takeWhile (fun t -> t < duration) |> Seq.toList
      data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "fake clock"
