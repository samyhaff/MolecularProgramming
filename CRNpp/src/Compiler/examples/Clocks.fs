namespace Examples

module Clocks =
    open ChemicalEngine
    open Rendering.Plotting

    let fakeClock phases =
      let duration = 2.0 * FakeClockSimulator.clockPhaseDuration * float phases
      let data = Reaction.toCRN [] [] |> FakeClockSimulator.watchConstRes phases duration

      let xs = Seq.initInfinite Simulator.constResTime |> Seq.takeWhile (fun t -> t < duration) |> Seq.toList
      data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "fake clock"
