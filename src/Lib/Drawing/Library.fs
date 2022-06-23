(*
    Author: Kristine Maria Klok Jørgensen
*)

module Drawing

open Plotly.NET

let draw style stateList =
    let makePlot list (steps, name) =
        let xs =
            seq {
                for i in 0.0 .. List.length steps do
                    yield i
            }

        let plot = Chart.Line(xs, steps, Name = name)

        let plot' =
            match style with
            | "smooth" -> Chart.withLineStyle (Shape = StyleParam.Shape.Spline) plot
            | "step" -> Chart.withLineStyle (Shape = StyleParam.Shape.Hvh) plot
            | s -> failwith "not a drawing option"

        plot' :: list

    Chart.combine (List.fold makePlot [] (List.filter (fun (steps, name) -> if name.Contains "osc" || name.Contains "tmp" || name.Contains "cmp"  then false else true) stateList))
    |> Chart.show

let speciesConcs species states = List.map (Map.find species) states

let drawStates res =
    draw "step" (List.map (fun s -> (speciesConcs s res, s)) (Seq.toList <| Map.keys res[0]))

