﻿// For more information see https://aka.ms/fsharp-console-apps
module Drawing

open Plotly.NET

let drawPlot func = 
    Chart.combine func |> Chart.show

let draw funcs (min:float) (max:float) = 
    let makePlot list (func, name) = 
        let xs = seq{for i in min..max do yield i}
        let ys = xs |> Seq.map func

        let plot = Chart.Scatter(xs, ys, mode=StyleParam.Mode.Lines, Name=name) 
                     |> Chart.withLineStyle(Shape =StyleParam.Shape.Spline)

        plot :: list
    drawPlot (List.fold makePlot [] funcs)

let drawSteps stepList = 
    let makePlot list (steps, name) = 
        let xs = seq{for i in 0.0 .. List.length steps do yield i}
        let plot = Chart.Scatter(xs, steps, StyleParam.Mode.Lines_Markers,Name=name) 
                    |> Chart.withLineStyle(Shape =StyleParam.Shape.Hvh)
        plot :: list
    Chart.combine (List.fold makePlot [] stepList)|> Chart.show