# Expression Splicing in C\# #

A couple of years ago, I was working on a C# project where we needed to manipulate C# expression trees at runtime. It's hard to do this by hand, but having played around with quasiquotes in Scala, I was inspired and wrote a tiny library to make this easier in C#. Recently, [a Reddit user was describing a similar need](https://www.reddit.com/r/AskProgramming/comments/8di1tw/does_anyone_like_programmatically_manipulating_c/), so I promised to dust off my code and write a blog post.

Though the Reddit user already knows what they're doing, to write this for a broader audience, I'll explain things from the beginning. Even if you have no idea what I'm talking about so far, you should be able to follow along.

## Background ##

At this point, most C# developers are probably accustomed to using lambdas in their code - anybody using Linq has done so. And anybody using Entity Framework has used expression trees, though they might not have realized it.

Consider the following two Linq queries:

    IEnumerable<int> e = ...;
    var result1 = e.Where(x => x < 10).ToList();

    IQueryable<int> q = ...;
    var result2 = q.Where(x => x < 10).ToList();

These look almost identical, so you might expect that they do the same thing. And if you're familiar with `IQueryable<T>`, you might know that it derives from `IEnumerable<T>`, further reinforcing that belief. You might therefore expect that the following will behave identically to the queryable version, above:

    IEnumerable<int> q_as_e = q;
    var result3 = q_as_e.Where(x => x < 10).ToList();

Unfortunately, that's not quite the case. This query will (probably) produce the same results, but depending on the particular `IQueryable`, it may do so in a very different way. For example, if `q` came from Entity Framework, then `result2` will be produced by generating an appropriate SQL query to filter the data on the database. On the other hand, `result3` will be produced by fetching all the data from the database table and filtering it in the C# application. 

It turns out that all the Linq methods, like `Where` and `Select`, are implemented as extension methods. When the extension method is called on an `IQueryable<T>`, the extension method lives on [a class called `Queryable`](https://docs.microsoft.com/en-us/dotnet/api/system.linq.queryable?view=netstandard-2.0); when called on an `IEnumerable<T>`, the extension method lives on [a class called `Enumerable`](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable?view=netstandard-2.0). The C# compiler will prefer to use the more specific extension method - if it knows that an expression is `IQueryable<T>`, it will prefer to use the `Queryable` extension methods over the analogous `Enumerable` extension method.

If you compare the signatures for `Where` in these two classes, you will quickly spot the differences:

    public static 
        System.Collections.Generic.IEnumerable<TSource> 
        Where<TSource> (
            this System.Collections.Generic.IEnumerable<TSource> source, 
            Func<TSource,bool> predicate
        );

    public static 
        System.Linq.IQueryable<TSource> 
        Where<TSource> (
            this System.Linq.IQueryable<TSource> source, 
            System.Linq.Expressions.Expression<Func<TSource,bool>> predicate
        );

Apart from what we might expect, the significant difference is the type of the `predicate` parameter. The `Enumerable` version takes a `Func<TSource, bool>`, as we would expect. But the `Queryable` version wraps that type up into an `Expression<Func<TSource, bool>>`. 

This small difference in the API ends up being the mechanism that allows tools like Entity Framework to do what they do. It enables a sort of run-time metaprogramming that is rarely present in statically-typed languages. It allows us to carry information about the static *structure* of our code all the way to runtime.

The C# compiler is able to treat any literal lambda either as a delegate or as an expression, depending on the expected type. For example, the following assignments are both valid:

               Func<int, bool>   f1 = x => x > 10;              // OK
    Expression<Func<int, bool>>  e1 = x => x > 10;              // OK

But note that only lambda literals can be treated this way. The following assignment to `f2` is valid, but the assignments to `e2` and `e3` result in compilation errors. 

               Func<int, int, int>   f2 = Math.Max;             // OK
    Expression<Func<int, int, int>>  e2 = Math.Max;             // error
    Expression<Func<int, int, int>>  e3 = f2;                   // error

When the compiler needs to convert a lambda literal into an `Expression<TDelegate>`, it doesn't compile the lambda literal into IL. Instead, it generates code that will build, at runtime, a data structure that resembles the lambda's body. For example, the assignment to `e1`, above, is identical to this code (I know; I checked the emitted IL):

    ParameterExpression p1 = Expression.Parameter(typeof(int), "x");
    Expression<Func<int, bool>> e1 = 
        Expression.Lambda<Func<int, bool>>(
            Expression.GreaterThan(
                p1, 
                Expression.Constant(10, typeof(int))
            ),
            p1);

So just to be clear, `e1` doesn't point to anything that's directly runnable - it's not pointing to a delegate. The `x => x > 10` lambda in fact has no compiled peer delegate in the generated assembly. `e1` instead points to a runtime data structure that *describes* the lambda literal that existed in the source code.

That also explains how Entity Framework is able to do what it does. Because it operates on instances of `IQueryable<T>`, all the Linq methods accept expression trees. The `Queryable` extension methods cooperate with the specific `IQueryable` instance to build up a representation of what Linq methods the user invoked, and the `IQueryable`'s `GetEnumerator` method ultimately interprets those expression trees. In the case of Entity Framework, that method will ultimately generate a SQL query and get results from the database.

One more detail: the library defines a base class `Expression` with a whole host of subclasses, including one called `LambdaExpression`. And `LambdaExpression` has a subclass called `Expression<TDelegate>`. `Expression<TDelegate>` is handled specially by the compiler. It represents a lambda literal of the given `TDelegate` type, and it is the only way to compel the compiler to generate code to the expression tree at runtime. That is to say, while this is valid:

    Expression<Func<int, bool>> e1 = x => x > 10;  // OK

This is not:

    LambdaExpression e2 = (int x) => x > 10;       // error

The second version doesn't invoke the expression tree functionality of the compiler.

In general, while every `Expression` node represents the root of *some* expression tree, every expression tree that is built by the compiler will be rooted by an instance of `Expression<...>`. 

## Splicing ##

The Reddit user was looking for a solution to translate this:

    x => x.IntProperty

into this:

    x => intermediateResults.Contains(x.IntProperty)

If we factor the first expression out of the second expression, we're left with something that looks like this:

    x => intermediateResults.Contains(???)

If we could write this in C#-like syntax, it might look something like this. I'm going to rename the parameters to avoid name collision:

    Expression<Func<T,int>>   substitutionExpr  =  y => y.IntProperty;
    Expression<Func<T, bool>> resultExpr        =  x => intermediateResults.Contains( ${substitutionExpr(x)} );

That is, the resulting expression is an ordinary expression with embedded references to other `Expression<TDelegate>` instances. As the expression tree is being built, we would like the embedded expressions to be expanded and inlined. In other words, `${substitutionExpr(x)}` should turn into `x.IntProperty`. Therefore, `resultExpr` should be equivalent to `intermediateResults.Contains(x.IntProperty)`.

Furthermore, we'd prefer to not need both expression literals to be defined in the same place. We'd like to something that could work like this:

    Expression<Func<T, bool>> MakeContainsExpression<T>(
        IQueryable<int> intermediateResults, 
        Expression<Func<T, int>> substitutionExpr) 
    {
        return x => intermediateResults.Contains( ${ substitutionExpr(x) } );
    }

I call this operation "expression splicing" (named after [rope splicing](https://en.wikipedia.org/wiki/Rope_splicing)) since we're taking two expressions, modifying them so that they're compatible with each other, then weaving them together.

## Problem ##

There's just one problem, but it's a big one: the `${ ... }` syntax is made-up. C# doesn't provide a way to automatically substitute expressions into other expressions. That won't work for us.

Fortunately, since expression trees are just runtime data structures, we can do the substitution ourselves. We can pass the template expression to some `Splice` function to do the actual splicing. However, we will need to invent a representation for placeholders in the template expression that is compatible with the syntax of C#. But we don't want our placeholders to be mistaken for legitimate expressions. Furthermore, we'd prefer something whose type safety can be checked at compile time. 

After thinking about the problem for a while, I had an insight: if we define bogus methods that nobody would ever have reason to legitimately call, we can embed calls to those bogus methods in our template expression to act as placeholders. So we can define a method with this signature:

    public static TResult Inline<T1, TResult>(this Expression<Func<T1, TResult>> substitutionExpr, T1 value1)
    {
        throw new Exception(
            "This method isn't meant to be called; it's meant to appear inside a template expression passed to Splice");
    }

And then we can use it in the template expression that we pass to `Splice`:

    Expression<Func<T, bool>> MakeContainsExpression<T>(
        IQueryable<int> intermediateResults, 
        Expression<Func<T, int>> substitutionExpr) 
    {
        return Splice((T x) => intermediateResults.Contains(substitutionExpr.Inline(x)));
    }

This approach has several nice properties:

1. It's type-safe: I can't call `Inline` with an argument of the wrong type. 
2. It's unambiguous: nobody would ever have a reason to call this `Inline` method from real code, so if it ever shows up in an expression tree, we know that it must represent a placeholder.
3. It's extensible: it's easy to add more versions of `Inline` that take a varying number of parameters. The replacement code can simply look for a call to any of these `Inline` methods. 

To define some terms, I'm going to refer to things like `substitutionExpr` as **substitution expressions** and the whole expression that we pass to `Splice` as the **template expression**. Note that the template expression contains embedded references to the substitution expressions. The expression returned from `Splice` is the **spliced expression**.

## Implementation ##

OK, so now all we need to do is to define a `Splice` function. It will walk the template expression and, whenever it encounters a call to `Inline`, it will inline the body of the substitution expression into the template expression.

Note that we can't just drop the substitution expression's body into the template expression directly. If we did that naively, we'd end up with something like this:

    x => intermediateResults.Contains(y.IntProperty);

`y` (which is a `ParameterExpression`) doesn't make sense on its own - it's a reference to a parameter that doesn't exist in this `LambdaExpression`.

When we have a subexpression within our template expression of the form:

    expr.Inline(arg)

We need to first transform the body of `expr`, replacing embedded parameter references with the actual arguments passed to `Inline`. Only then can we drop the transformed substitution expression into the template expression.

To reiterate, this means that we're going to rewrite both the template expression as well as the substitution expressions. 

1. We'll rewrite the template expression to eliminate calls to `Inline`
2. For every time that a substitution expression is inlined, we need to replace references to its own parameters with references to the `Inline` arguments.

The .NET Framework provides a convenient mechanism to rewrite expression trees in the form of the class [`ExpressionVisitor`](https://docs.microsoft.com/en-us/dotnet/api/system.linq.expressions.expressionvisitor?view=netstandard-2.0). The default behavior of its `Visit` method is to generate an exact duplicate of the input expression tree, but subclasses can override its various `VisitX` methods to do something different.

So let's briefly skip to the simpler replacement that we need to do: replacing parameter references in substitution expressions. We can define a simple subclass of `ExpressionVisitor` that will do this:

    class ParameterReplacer : ExpressionVisitor
    {
        private readonly Dictionary<ParameterExpression, Expression> _formalParameterSubstitutions;

        public ParameterReplacer(Dictionary<ParameterExpression, Expression> formalParameterSubstitutions)
        {
            _formalParameterSubstitutions = formalParameterSubstitutions;
        }

        protected override Expression VisitParameter(ParameterExpression node)
        {
            if (_formalParameterSubstitutions.TryGetValue(node, out var actualValue))
            {
                return actualValue;
            }

            return base.VisitParameter(node);
        }
    }

You can call it like this:

    Expression<Func<int, int>> doubleExpr = x => x * 2;
    
    var substitutions = new Dictionary<ParameterExpression, Expression>();
    substitutions[doubleExpr.Parameters[0]] = Expression.Add(
            Expression.Constant(3),
            Expression.Constant(4));

    var rewritten = new ParameterReplacer(substitutions).Visit(doubleExpr.Body);

Which will produce the following expression tree:

    (3 + 4) * 2

Note that we're not passing `doubleExpr` (a `LambdaExpression`) itself to the `Visit` method; rather, we're using the `ParameterReplacer` to rewrite just its body. This makes sense. When we substitute back into the template expression, we don't want to insert a lambda. We want to insert the rewritten body of the lambda. 

Armed with that, we can now write the code to do replacement in the template lambda:

    class SpliceRewriter : ExpressionVisitor
    {
        protected override Expression VisitMethodCall(MethodCallExpression node)
        {
            if (node.Method.DeclaringType == typeof(ExpressionSplicer) && node.Method.Name == "Inline")
            {
                // `node.Arguments[0]` (i.e. the `this` parameter to `Inline` will be the substitution expression
                // and the remainder of `node.Arguments` will be the arguments we want to provide to that
                // substitution expression.
                LambdaExpression expressionToInsert =
                    Expression.Lambda<Func<LambdaExpression>>(node.Arguments[0]).Compile()();

                var substitutionArguments = node.Arguments.Skip(1);
                var parameterArgumentPairs = expressionToInsert.Parameters
                    .Zip(substitutionArguments, (parameter, argument) => new { parameter, argument });
                var substitutions = parameterArgumentPairs.ToDictionary(x => x.parameter, x => x.argument);

                var parameterReplacer = new ParameterReplacer(substitutions);
                return parameterReplacer.Visit(expressionToInsert.Body);
            }

            return base.VisitMethodCall(node);
        }
    }

You can see that this visitor only handles calls to `Inline` methods specially. Other calls are not rewritten.

One thing that might seem suspicious is the line that calls `LambdaExpression.Compile` (and then immediately invokes the resulting delegate). You might be wondering why that's being done and about the runtime cost of calling that method. 

To answer the first question, it's important to think about the value that we'll find in `node.Arguments[0]`. You might expect it to be an instance of `LambdaExpression`, but that's not necessarily the case. More likely, it's some other kind of `Expression` node that represents some chunk of code that can produce a `LambdaExpression`. In the `MakeContainsExpression` example above, in fact, that will actually be a `FieldExpression`, which is an internal subclass of `MemberExpression`. (If this seems odd, it's due to the way that the C# compiler represents closed-over variables in lambdas - captured variables are transformed into fields on some generated class.)

In a case like this, compiling the expression node into a parameterless lambda is the most direct way to get its value (though see the "Future Work" section, below).

To answer the second question, I'd mention that this pattern of dynamically compiling a `LamdbaExpression` and then executing it is already used by the framework. Specifically, `EnumerableQuery<T>.GetEnumerator` does something very similar. It's invoked whenever you use an `IEnumerable<T>` as if it was an `IQueryable<T>`:

    new List<int> { 1, 2, 3 }.AsQueryable().Select(x => x * 2).ToList();

When `ToList` is called, it will turn around and call `GetEnumerator` on an instance of `EnumerableQuery`, and that will assemble a `LambdaExpression`, compile it, and execute the resulting delegate.

With both expression rewriting visitors written, I can now show you the `Splice` implementation itself:

    public static Expression<Func<T1, TResult>> Splice<T1, TResult>(
        Expression<Func<T1, TResult>> templateExpr)
    {
        return new SpliceRewriter().VisitAndConvert(templateExpr, "Splice");
    }

That's it! That's the core implementation of expression splicing. My implementation is just about 100 lines sans comments, and a good chunk of that is boilerplate (such as defining multiple variants of `Inline` and `Splice`).

---

The example `MakeContainsExpression` above already showed how you would use it, but let me show you another example. It can be useful to construct expression trees that include a bunch of subexpressions that are all combined with `&&`. When we know the expressions up front, that's easy - you can just create the expression tree directly:

    Expression<Func<int, bool>> = x => x > 0 && x < 10;

When the expressions aren't known up-front, but the structure is known, you can use the expression splicer to generate an expression:

    static Expression<Func<T, bool>> And2<T>(
        Expression<Func<T, bool>> left, 
        Expression<Func<T, bool>> right)
    {
        return Splice((T x) => left.Inline(x) && right.Inline(x));
    }

But what if we we're not even sure up front how many expressions we need? Maybe we want to combine two expressions, or maybe we want to combine five. We could make multiple overloads of `And` that take different numbers of parameters. Or, since this is all just runtime data structure manipulation, we can use our ordinary data structure manipulation tools:

    static Expression<Func<T, bool>> And<T>(params Expression<Func<T, bool>>[] exprs)
    {
        if (exprs.Length == 0)
        {
            return x => true;
        }

        return exprs.Aggregate(And2);
    }

Given that we've already written a function that can combine two arbitrary expressions with `&&`, it's easy to write a function that can combine an arbitrary number of expressions. And since `&&` is left-associative, `Aggregate` is a good match. (If we wanted to handle a right-associative construct, we'd need to do it by hand - `Aggregate` walks the collection from left-to-right, and as far as I know, there's no right-biased version of this operation in Linq.)

## Future Work ##

This is pretty close to the implementation that we're using in production. To the best of my knowledge, it's been working well for several years. But of course there are always ways that it could be improved. 

Shown here was just one variant of `Splice` (which takes one parameter) and just one variant of `Inline` (which also takes just one parameter). It's possible to define variants of `Splice` which take 0 or more parameters, and it's possible to define variants of `Inline` which do the same. You would want higher-arity `Inline` methods when you want to support substitution expressions that take more parameters, and you'd want higher-arity `Splice` methods when you want to support template expressions that take more parameters. 

When dealing with nested Linq queries, it's sometimes desirable not to inline the substitution expression into the template, but rather to merely reference it - for example, when calling one of the extension methods on `Queryable`, which expect `Expression<Func<...>>`. But as it turns out, inside a lambda, it's sometimes necessary to call extension methods on `Enumerable` (such as `Select`), and those expect `Func<...>`. Now, since we have an `Expression<Func<...>>` in hand, we *ought* to be able to pass it as the argument to `Select`, since an `Expression<Func<...>>` represents a lambda literal and lambda literals are generally usable as delegates. But we'd need to do some massaging of the `Expression` to make that work, and that massaging represents a different kind of substitution (in our case, we called that operation `Unwrap`, though I'm not totally sold on that name).

The expression splicer currently looks very specifically for methods called `Inline` that are defined directly in the `ExpressionSplicer` class. That works fine as long as the person writing `ExpressionSplicer` defined enough variants up front. If the user of `ExpressionSplicer` needs a variant that wasn't part of the original library, they're out of luck. It would be possible to use a custom attribute to indicate which methods are placeholder markers, allowing a user to add support for arities beyond those shipped in the library.

Similarly, this implementation handles just one kind of expression substituting. Other kinds are useful in practice, like the aforementioned `Unwrap` substitution. The same custom attribute could also refer to an implementation method to do the actual substitution, similar to how NUnit uses `TestCaseSource` to point to methods and properties that produce test data.

As mentioned above, though the framework uses `LambdaExpression.Compile` already, there may be some concerns about the runtime cost of invoking the compiler just to extract some constant data from the expression tree. It would be possible to replace the dynamic compilation with a function that walks the expression tree directly, though that function would need to deal with all the possible expression tree structures that would arise in the wild.

To take that even further, expression trees have an additional runtime cost. At runtime, a new copy of the expression tree is generated every time the expression tree is needed. So given the following code:

    void DoSomething()
    {
        // ...
        Expression<Func<string, int>> strLengthExpr = x => x.Length;
        // ...
    }

Then the same expression tree will be built, from scratch, every time `DoSomething` is called. If we intend to re-use the same template expression many times, we could potentially pre-process the template expression. This would require that we change our API, though, since we would no longer be able to use closed-over substitution expressions directly in our template expression.

Even if we were to generate the template expression tree just once, we still need to walk it every time we call `Splice` to find the occurrences of `Inline`. Part of the template pre-processing could find and remember where these placeholders occur. In fact, it could also extract subexpressions from the template expression that do not participate in substitution; these subexpressions could be cached and re-used directly. 

Taking that to its logical conclusion, it would be possible to, at runtime, analyze a template expression tree and generate a delegate which would perform the substitution described by the template. In other words, the C# compiler would emit code to generate the template expression, and we would run that code once. But we would then analyze that resulting expression tree to generate a new delegate at runtime. Our delegate's instructions would closely resemble the instructions in the compiler-generated code (the instructions that produced the original expression tree), except that it would also perform expression substitution as it builds the spliced expression.

Perhaps the best future improvement, though, would be to completely eliminate the need to do any of this at runtime. In the same way that the C# compiler now handles string interpolation, it would be neat to see a concrete C# syntax to splice expression trees together (like the fantasy `${ ... }` syntax above). But I don't honestly expect that to happen. The C# language team only has so many hours in the day, and adding yet another special syntax would only serve to make the language more complex. And while string interpolation is useful to everybody, expression splicing is a niche need at best. Still, that's not so bad. That we were able to build something ourselves just highlights how powerful and flexible the existing mechanism is. 