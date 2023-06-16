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
      let steps= [
        toCRN [([name A, 1], 1.0, [name A, 1; name B, 1])] species;
        toCRN [([name A, 1], 1.0, [name A, 1; name C, 1])] species;
      ]
      let stepCount = List.length steps
      let duration = 2.0 * float Clock.stepPeriod * FakeClockSimulator.clockPhaseDuration * float stepCount
      let data = steps |> FakeClockSimulator.watchConstRes duration |> FakeClockSimulator.filterClockSpecies

      let xs = Seq.initInfinite Simulator.constResTime |> Seq.takeWhile (fun t -> t < duration) |> Seq.toList
      data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "fake clock"
