open System
open System.Globalization

let add x y = x + y
let subtract x y = x - y
let multiply x y = x * y

let divide x y =
    if y = 0.0 then None
    else Some (x / y)

let power x y = Math.Pow(x, y)

let sqrt x =
    if x < 0.0 then None
    else Some (Math.Sqrt x)

let sin x = Math.Sin x
let cos x = Math.Cos x
let tan x = Math.Tan x

let printOptionFloat label result =
    match result with
    | Some v -> printfn "%s: %f" label v
    | None -> printfn "%s: Ошибка вычисления" label

let readFloat prompt =
    let rec read () =
        printf "%s" prompt
        let input = Console.ReadLine()
        match Double.TryParse(input, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> v
        | _ ->
            printfn "Ошибка ввода. Пожалуйста, введите число."
            read()
    read()

let rec readOperation () =
    printfn "\nВыберите операцию:\n\
             1) Сложение (+)\n\
             2) Вычитание (-)\n\
             3) Умножение (*)\n\
             4) Деление (/)\n\
             5) Возведение в степень (^)\n\
             6) Квадратный корень (√)\n\
             7) Синус (sin)\n\
             8) Косинус (cos)\n\
             9) Тангенс (tan)\n\
             0) Выход"
    printf "Введите номер операции: "
    match Console.ReadLine() with
    | "1" -> Some "add"
    | "2" -> Some "subtract"
    | "3" -> Some "multiply"
    | "4" -> Some "divide"
    | "5" -> Some "power"
    | "6" -> Some "sqrt"
    | "7" -> Some "sin"
    | "8" -> Some "cos"
    | "9" -> Some "tan"
    | "0" -> None
    | _ ->
        printfn "Некорректный ввод. Попробуйте снова."
        readOperation()

let calculator () =
    let rec loop () =
        match readOperation() with
        | None -> printfn "Выход..."
        | Some op ->
            let getOperands = 
                match op with
                | "sqrt" | "sin" | "cos" | "tan" -> 
                    fun () -> (readFloat "Введите число (или угол в градусах для тригонометрии): "), None
                | _ ->
                    fun () -> 
                        let x = readFloat "Введите первое число: "
                        let y = readFloat "Введите второе число: " 
                        (x, Some y)

            let x, maybeY = getOperands()

            let degToRad deg = deg * Math.PI / 180.0

            let result =
                match op with
                | "add" -> Some (add x (maybeY.Value))
                | "subtract" -> Some (subtract x (maybeY.Value))
                | "multiply" -> Some (multiply x (maybeY.Value))
                | "divide" -> divide x (maybeY.Value)
                | "power" -> Some (power x (maybeY.Value))
                | "sqrt" -> sqrt x
                | "sin" -> Some (sin (degToRad x))
                | "cos" -> Some (cos (degToRad x))
                | "tan" -> Some (tan (degToRad x))
                | _ -> None

            printOptionFloat "Результат" result
            loop()
    loop()

[<EntryPoint>]
let main argv =
    printfn "=== Калькулятор ==="
    calculator()
    0
