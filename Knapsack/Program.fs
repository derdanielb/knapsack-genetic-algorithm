﻿open System.Threading

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.





module Data =
    let itemsValue = [|0 .. 9|]
    let itemsWeight = [|0.0 .. 0.5 .. 5.0|]
    let knapsackSize = 5
    let individualAmount = 10
    let generations = 10
    let calculateFitness = 
        0 //TODO implement

        


module Initialization =
    let checkContains (x : int[]) y  =
        let res = [|false|]
        for i = 0 to x.Length-1 do
            if( x.[i] = y) then
                res.[0] <- true
        res.[0]

    let generateRandomIndividual size =
        // needs sleep because of the weird bug that creates the same individual over and over again. Sleep somehow sloves it
        Thread.Sleep(100) 
        let individual = Array.create size -1
        let rnd = System.Random()
        for i = 0 to individual.Length-1 do
            while individual.[i] < 0 do
                let rand = rnd.Next Data.itemsValue.Length
                if(not(checkContains individual rand)) then
                    individual.[i] <- rand
        individual


module GeneticSelection = 
    let individuals = [| for i in 0 .. Data.individualAmount -> Initialization.generateRandomIndividual Data.knapsackSize |]
    
    

            
        


[<EntryPoint>]
let main argv = 
    //for i = 0 to 10 do 
    //    Thread.Sleep(50)
    printfn "%A" GeneticSelection.individuals
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code