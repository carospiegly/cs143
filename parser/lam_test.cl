


class Lambda inherits Expr {
	-- We allow variables to be reused
	substitute(x : Variable, e : Expr) : Expr {
	if x = arg then
		self
	else
		let new_body : Expr <- body.substitute(x, e),
		new_lam : Lambda <- new Lambda in
		new_lam.init(arg, new_body)
	fi
	};
};
