// For more information see https://aka.ms/fsharp-console-apps
module Drawing

open Plotly.NET

let makePlot list (func, name) = 
    let xs = seq{for i in 0.0..10.0 do yield i}
    let ys = xs |> Seq.map func

    let plot = Chart.Scatter(xs, ys, mode=StyleParam.Mode.Lines, Name=name) 
                 |> Chart.withLineStyle(Shape =StyleParam.Shape.Spline)

    plot :: list

let drawPlot func = 
    Chart.combine func |> Chart.show

let draw funcs = 
    drawPlot (List.fold makePlot [] funcs)

// draw [((fun x -> x*x), "first"); ((fun x -> x*x*x), "second")]