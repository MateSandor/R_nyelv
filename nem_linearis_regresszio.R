nem_linearis_regresszio = function(x, y){

	u = x
	v = log(y)

	print("x:")
	print(x)
	print("u:")
	print(u)
	print("v:")
	print(v)

	if(length(u) != length(v))
	{
		return('Azonos hosszúnak kell lennie az u-nak és v-nek báttya!');
	}

	# u átlag
	mean_u = 0;
	n = length(u); #= length(v)
	for(i in 1:n)
	{
		mean_u = mean_u + u[i];
	}
	mean_u = mean_u / n;

	# v átlag
	mean_v = 0;
	for(i in 1:n)
	{
		mean_v = mean_v + v[i];
	}
	mean_v = mean_v / n;

	a = 0;
	b = 0;
	sum1 = 0;
	sum2 = 0;
	
	# "a" tört számláló
	for(i in 1:n)
	{
		sum1 = sum1 + ((u[i]-mean_u)*(v[i]-mean_v));
	}
	
	# "a" tört nevező
	for(i in 1:n)
	{
		sum2 = sum2 + ((u[i]-mean_u)^2);
	}
	
	a = sum1 / sum2;

	b = mean_v - (a*mean_u)

	cat("Az a értéke: ",a,"\n");
	cat("A b értéke: ",b,"\n");
	# megjelenítés
	plot(u,v);
	abline(b,a);
}