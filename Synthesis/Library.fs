module Synthesis

let abelar a = (a > 12 && a < 3097 && a % 12 = 0)
   //failwith "Not implemented"

let area a b =
    match (a >= 0.00 && b >= 0.00) with 
    | true -> (a*b)/2.0
    | false -> failwith "Base or height cannot be negative"
                
    //failwith "Not implemented"

let zollo x =
    match x<0 with
    |true -> x*(-1)
    |false -> x*2

   // failwith "Not implemented"

let min x y =
    match x>y with 
    |true -> y
    |false -> x
   // failwith "Not implemented"

let max x y =
     match x<y with 
    |true -> y
    |false -> x

    //failwith "Not implemented"

let ofTime a b c =((a*3600) + (b*60) + (c*1))
    //failwith "Not implemented"

let toTime x =
    match x <= 0 with 
    |true -> 0,0,0
    |_ ->
        let hrs = x / 3600
        let min = (x / 60) % 60
        let sec = (x - hrs * 3600) % 60 
        hrs,min,sec

    //failwith "Not implemented"

let digits d =
    let rec Count n  acc =
        match n=0 with 
        |true -> acc
        |false -> Count (n/10) (acc+1)
    match d>0 || d<0 with
    |false -> 1
    |true -> Count d 0


    //failwith "Not implemented"

let minmax (a,b,c,d) =
   let minval = min a b |> min c |> min d
   let maxval = max a b |> max c |> max d
   (minval,maxval)
   // failwith "Not implemented"

let isLeap x =
    match x < 1582 with 
    |true -> failwith("Year is before 1582")
    |false ->
        match x%4=0 && not (x%100= 0) with 
        |true -> true 
        |_ -> 
            match x%400=0 with 
            |true -> true
            |_ -> false
    //failwith "Not implemented"

let month x =
    match x with 

    |1 -> ("January",31)
    |2 -> ("February",28)
    |3 -> ("March",31)
    |4 -> ("April",30)
    |5 -> ("May",31)
    |6 -> ("June",30)
    |7 -> ("July",31)
    |8 -> ("August",31)
    |9 -> ("September",30)
    |10 -> ("October",31)
    |11 -> ("November",30)
    |12 -> ("December",31)
    |_ -> failwith("Not valid month")
    
    //failwith "Not implemented"

let toBinary num = 
    match num < 0 with 
    |true -> failwith("Negative number is invalid")
    |false -> 
        match num=0 with 
        |true -> "0"
        |_ ->
            let rec Count n result =
                match n>0 with
                |false -> result
                |_ ->
                    match n%2 with 
                    |0 -> Count (n/2) ("0" + result)
                    |_ -> Count (n/2) ("1" + result)
            Count num ""  

   // failwith "Not implemented"

let bizFuzz num =
    let rec div x (acc1, acc2, acc3) =
        match num < x with
        |true ->(acc1,acc2,acc3)
        |false ->
            match x%3=0 && x%5=0 with 
            |true ->div (x+1) (acc1 + 1, acc2+1, acc3+1)
            |false ->
                match x%3=0 with 
                |true -> div (x+1) (acc1+1,acc2,acc3)
                |false ->
                    match x%5=0 with 
                    |true -> div (x+1) (acc1, acc2 + 1, acc3)
                    |false -> div (x+1) (acc1 , acc2 , acc3)
    match num < 1 with
    |true -> (0,0,0)
    |false -> div 1 (0,0,0)
    
    //failwith "Not implemented"

let monthDay d y =
    match d > 0 && d <= 366 && y >= 1582 with
    |false -> failwith "Entered invalid value"
    |true ->
        let rec monthFinder v m acc = 
            let mon,day = month m 
            match isLeap y = true with
            |true ->
                match mon = "February" with
                |true ->
                    match v > acc && v<= (acc+day+1) with
                    |true -> mon
                    |false -> monthFinder v (m+1) (acc+day+1)
                |false ->
                    match v > acc && v <= (acc + day) with 
                    |true -> mon
                    |false -> monthFinder v (m+1) (acc+day)
            |false ->
                match isLeap y = false && d < 366 with
                |false -> failwith "Entered invalid value"
                |true  ->
                    match v > acc && v <= (acc+day)  with
                    |true -> mon
                    |false ->monthFinder v (m + 1) (acc + day)
        let mon, day = month 1
        match d > 0 && d <= day with 
        |true -> "January"
        |false ->
            monthFinder d 2 31
                    
  //  failwith "Not implemented"

let coord _ =
    failwith "Not implemented"