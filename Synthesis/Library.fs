module Synthesis

let abelar a = (a>12) && (a<3097) && (a%12=0)

let area b h =
   match b<0.0 || h<0.0 with
   | true -> failwith "Invalid"
   | false -> (b * 1.0/2.0) * h


let zollo a =
    match a<0 with
    | true -> a/(-1)
    | false -> 2*a

let min a b =
    match a<b with 
    | true -> a
    | false -> b
let max a b =
    match a>b with 
    | true -> a
    | false -> b

let ofTime a b c = (a*3600) + (b*60) + c

let toTime a =
    let hrs = a/3600
    let min = (a-(hrs*3600))/60
    let sec = a - (hrs*3600) - (min*60)
    match a<0 with
    |true -> 0,0,0
    |false -> hrs,min,sec
    

let digits a =
    let rec numbers div acc=
        match (div<10 && div>(-10)) with 
        |true -> acc
        |false -> numbers (div / 10) (acc+1)
    numbers a 1

let minmax turple =
    failwith "Not implemented"

let isLeap a =
    match (a<1582) with
    |true -> failwith "Invalid"
    |false ->match (a%4=0)  with
                |false -> false
                |true -> match (a%100=0)  with
                            |false ->true
                            |true ->match (a%400=0)  with
                                    |false ->false
                                    |true ->true
            
    

let month a =
    match a with 
    |1 -> "January", 31
    |2 -> "February", 28
    |3 -> "March", 31
    |4 -> "April", 30
    |5 -> "May", 31
    |6 -> "June", 30
    |7 -> "July", 31
    |8 -> "August", 31
    |9 -> "September", 30
    |10 -> "October", 31
    |11 -> "November", 30
    |12 -> "December", 31
    |_ -> failwith "Invalid"
    

let toBinary a = match a<0 with 
                |true -> failwith "failed"
                |false ->match a=0 with 
                        |true -> "0"
                        |false ->
                            let rec binary num acc = 
                                match num=0 with
                                |true -> acc
                                |false -> match (num%2=0) with 
                                        |true -> binary (num/2) ("0"+acc)
                                        |false -> binary (num/2) ("1"+acc)
                            binary a ""

let bizFuzz a =
    let rec divisor start num div acc = 
        match start>num with 
        |true-> acc
        |false -> match (start%div=0) with
                  |true -> divisor (start+1) num (div) (acc+1)
                  |false ->divisor (start + 1)num (div) (acc)

    let x = divisor 1 a 3 0
    let y = divisor 1 a 5 0

    let rec divisor2 start num acc = 
        match start>num with 
        |true-> acc
        |false -> match (start%3=0 && start%5=0) with
                  |true -> divisor2 (start+1) (num) (acc+1)
                  |false -> divisor2 (start+1) (num) (acc)

    let z = divisor2 1 a 0
    x,y,z

let monthDay d y =
    match (d>1 and d<1)

let coord _ =
    failwith "Not implemented"