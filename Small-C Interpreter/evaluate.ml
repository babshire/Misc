type data_type = 
	|Undefined
	|N of int  
;; 


let rec get_value env x = match x with
	|Id (z) -> let d = Hashtbl.find env z in let p = (match d with N (num) -> num) in p 
	|Num (x) ->  x 
	|Paren (ast) -> get_value env ast 
	|Sum (exp1, exp2) ->  (get_value env exp1) + (get_value env exp2) 
	|Mult (exp1, exp2) -> (get_value env exp1) * (get_value env exp2) 
	|Pow (exp1, exp2) ->  
		let rec calculate_power x counter =  
			if  counter = 1 then 
				x  
			else  
				x * calculate_power x (counter-1)
		in calculate_power (get_value env exp1) (get_value env exp2)
	|Greater (exp1, exp2)-> if ((get_value env exp1) > (get_value env exp2)) then  1 else -1 
	|Equal (exp1, exp2) -> if ((get_value env exp1) == (get_value env exp2)) then  1  else -1 
	|Less (exp1, exp2) -> if ((get_value env exp1) < (get_value env exp2)) then 1 else -1 


;;

let rec eval env x =  match x with 
	|Fun (_,_,_,states) -> eval env states; env;
	|List p -> (match p with
		|[] -> env
		|h::t -> eval env h; eval env (List t))
	|Define(_,name) -> 
		let id = (match name with Id (x) -> x) in
		if (Hashtbl.mem env id) then
			raise (IllegalExpression "Variable has been defined already")
		else
			Hashtbl.add env id Undefined; env;
	|Assign(name,exp) -> 
		let id = (match name with Id (x) -> x) in
		let v = get_value env exp in 
		if (Hashtbl.mem env id) == false then 
			raise (IllegalExpression "Variable has never been defined") 
		else 
			Hashtbl.replace env id (N v); env;
	|If (cond, if_branch, else_branch) ->  
		if ((get_value env cond) == 1) then  
			let x = eval env if_branch in env; 
		else 
			let y = eval env else_branch in env;
	|While (cond, body) ->  
		let rec exec_while env cond body =  
			let a = (get_value env cond) in  
			if (a != 1) then 
				env 
			else 
				(let p = eval env body in let q = exec_while env cond body in env);
		in exec_while env cond body;
	|Print (exp) -> (match exp with 
		|Id (q) -> let d = Hashtbl.find env q in  
			let x = (match d with N (num) -> num) in 
			print_int x; print_string "\n"; env; 
		|_ -> print_int (get_value env exp); print_string "\n"; env) 


;;

	 
