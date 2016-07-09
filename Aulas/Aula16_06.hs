Semântica de linguagens Funcionais

E := n | x | b | E op E | If E then E else E
	| E E | Func x : T => E | let x:T = E in E

	x ℰ ℤ 
	b ℰ {True, False}
	op ::= + | - | * | / | ^ | v | <= | ...
	T:= Int | Bool | T -> T
	v:= n | b | Fun x:T => E

Uma unica categoria Sintática
Permite Escrever programas errados, E + "2+False"
O sistema de tipos (análise semântica)
Deve rejeitar esse tipo de programa

> 1
1
> 1 + If True then 1 else 2
2
>(fun x: INT => x + x) 3
> let x:Int= 3 + 3 in x + x
12
> (let x:Int= 2 in x + x)+(let x:Int=1 in x + x)
24
> (func x:Int => let y:Int=3+x in y + x) 2
7
> (fun x:Int->Int = x 1) (fun x:Int=>x+1)
2
--calculo lambda
(𝜆x * x + 1) 2
2+1
3
--função soma
> (fun x: Int=>fun y: Int => x+y)3)2
(fun y:Int=>3+y)2
3+2
5


Uma função é um valor da linguagem: Uma expressão pode avaliar para uma função, funções podem consumir funções e devolver
funções como a resposta da computação

O programa, antes de ser executada, deve passar pela análise semântica(sistema de tipos) para verificar se os programas
respeitam os tipos das operações

 Semântica Small Step da Linguagem Funcional
Int -> Int x = (y -> y + 1)

> (fold (+) . map (+1)) [2,3,4]
12

A Semântica Small Step avalia uma expressão em zero ou mais passos para um valor
	E >* v

Expressões lógicas e aritméticas são como antes:
	E1 ↦ E1				: Soma1
	E1 + E2 ↦ E1' + E2

	E ↦ E'			: Soma2
	n + E ↦ n+E'

	n = n1 + n2 	: Soma3
	n1 + n2 ↦ n

	E1 ↦ E1'		: AP1
	E1 E2 ↦ E1' E2

	E ↦ E'		: AP2
	v E ↦ v E'

							: AP3
	(fun x:T=E) v ↦ {v|x} E

{v | x} E : Substitue todas as ocorrências de x em E por v

(fun x: Int ↦ x + 10) 3
	AP3
	->
{3|x} x + 10
	=
3+10
	Soma3
	->
	13

--Regra do Let

____E1 ↦ E1'__________________________ :let1
let x:T = E1 in E2 ↦ let x:T=E1' in E2

___________________ :let2
let x:T = v in E2 ↦ 

let x:Int = 3+3 in x + x ↦* 12
let x:Int = 3+3	in x + x
	 A
	->
let x:Int = 6 in x+x
	let2
	->
	6+6
	Soma3
	->
	12

A) 
			:Soma3
______3+3↦6________________________________________:let1
let x:Int = 3 + 3 in x+x -> let x: Int = 6 in x+x 

((fin x:Int=>fun y:Int => x + y) 2) 3
	↦*
	5
_______________________________
((fun x:Int=>fun y:Int=>x+y)2)3
	A		
	↦
(fun y:Int => 2+y)
	AP3
	->
	2+3
	Soma3
	->
	5

A)

________________________________:AP3
(fun x:Int => fun y:Int => x+y) 2 -> fun y:Int=>2+y___:AP1 
(fun x:Int => fun y:Int => x+y) 2)3 -> 
(fun y:Int => 2+y)3

(If (True ^ False) Then (fun x:Int=> x + 10) else (fun y:Int=>x+20)) 2+2 ↦* 24
________________________E1__________________________________________ _E2_
 


