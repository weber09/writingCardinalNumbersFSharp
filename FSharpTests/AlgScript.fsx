let writeNumbers nr =
    let unity = ["zero"; "um"; "dois"; "três"; "quatro"; "cinco"; "seis"; "sete"; "oito"; "nove"; "dez"; "onze"; "doze"; "treze"; "quatorze"; "quinze"; "dezesseis"; "dezessete"; "dezoito"; "dezenove"]
    let decimals = [""; "cem"; "vinte"; "trinta"; "quarenta"; "cinquenta"; "sessenta"; "setenta"; "oitenta"; "noventa"]
    let hundreds = [""; "cento"; "duzentos"; "trezentos"; "quatrocentos"; "quinhentos"; "seissentos"; "setecentos"; "oitocentos"; "novecentos"]
    let thousand = "mil"

    let e numberStr =
        match numberStr with
            | "" -> numberStr
            | _ -> numberStr + " e "

    let writeUnitary (nr : int, numberStr : string) =
        if(nr = 0 && numberStr <> "") then
            numberStr
        else if (nr < 20) then
                e numberStr + unity.[nr]
            else
                numberStr

    let writeDecimalsNr (nr : int, numberStr : string) =
        if( nr = 0 && numberStr <> "") then
            (nr, numberStr)
        else if(nr < 20) then
                (nr, numberStr )
            else
                let decimalsNr = nr / 10
                let number = nr - decimalsNr * 10
                (number, e numberStr + decimals.[decimalsNr])

    let writeHundredsNr (nr : int, numberStr : string) = 
        let hundredsNr = nr / 100
        let number = nr - hundredsNr * 100
        match hundredsNr with
            | 0 -> (nr, numberStr)
            | _ ->  match number with
                      | 0 -> if(hundredsNr = 1) then 
                                  (number, e numberStr + decimals.[hundredsNr]) 
                             else 
                                   (number, e numberStr + hundreds.[hundredsNr])
                      | _ -> (number, e numberStr + hundreds.[hundredsNr])
        

    let writeThousandsNr nr = 
        let thousandNr = nr / 1000
        let number = nr - thousandNr * 1000
        match thousandNr with
            | 0 -> (number, "")
            | 1 -> (number, thousand)
            | _ -> (number, (writeUnitary (thousandNr, "")) + " " + thousand)

    nr
    |> writeThousandsNr
    |> writeHundredsNr
    |> writeDecimalsNr
    |> writeUnitary
        