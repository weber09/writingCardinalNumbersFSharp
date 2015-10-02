// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let unity = ["zero"; "um"; "dois"; "três"; "quatro"; "cinco"; "seis"; "sete"; "oito"; "nove"; "dez"; "onze"; "doze"; "treze"; "quatorze"; "quinze"; "dezesseis"; "dezessete"; "dezoito"; "dezenove"]
let decimals = [""; "cem"; "vinte"; "trinta"; "quarenta"; "cinquenta"; "sessenta"; "setenta"; "oitenta"; "noventa"]
let hundreds = [""; "cento"; "duzentos"; "trezentos"; "quatrocentos"; "quinhentos"; "seissentos"; "setecentos"; "oitocentos"; "novecentos"]
let thousand = "mil"

let writeNumber nr : string =
    let mutable number = nr
    let mutable numberStr = ""
    
    let thousandNr = number / 1000
    if(thousandNr <> 0) then
        if(thousandNr = 1)  then
            numberStr <- thousand
        else
            numberStr <- unity.[thousandNr] + " " + thousand
    
    number <- number - thousandNr * 1000
    
    if(number <> 0 || numberStr = "") then
        let hundredNr = number / 100
        number <- number - hundredNr * 100
        if(hundredNr <> 0) then
            if(number = 0) then
                if(numberStr <> "") then
                    numberStr <- numberStr + " e "
    
                numberStr <- numberStr + decimals.[hundredNr]
            else
                if(numberStr <> "") then
                    numberStr <- numberStr + " "

                numberStr <- numberStr + hundreds.[hundredNr]
    
        if(number < 20) then
            if(numberStr <> "") then
                numberStr <- numberStr + " e "
            numberStr <- numberStr + unity.[number]
        else
            let decimalNr = number / 10
            number <- number - decimalNr * 10
            if(number = 0) then
                if(numberStr <> "") then
                    numberStr <- numberStr + " e "
                numberStr <- numberStr + decimals.[decimalNr]
            else
                if(numberStr <> "") then
                    numberStr <- numberStr + " e "
                numberStr <- numberStr + decimals.[decimalNr] + " e " + unity.[number]
    numberStr

printf "Digite um número: \n"
let number = int32(System.Console.ReadLine())
let str = writeNumber number

printf "\n%s\n" str

        