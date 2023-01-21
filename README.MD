js to hs 
hs to js 
code to money

        Lightweight package which runs any javascript statement or statements by
        first parsing some Javascript statements into JSContext that can be interacted with by
        ensuing commands

        Note that this is not meant to parse any Javascript in existence but rather syntactically correct
        JS.

        This functionality originally was apart of the scrappy package but has been separated out into its own
        package. For this reason the design is around parsing and handling the execution of JS such as
        when browser events happen much like the jsdom npm package and so early versions may not have
        a safe API or typesystem for adding in your own JS however this can simply be done by writing
        raw JS using `eval`. Contributions welcome. 
		
		Over time this will fake less and less about Browser APIs but the overall strategy is just minimize
		damage to overall effects/process until such real behavior is necessary
