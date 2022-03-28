import cats.Eval
val now = Eval.now(Math.random + 1000)
val always = Eval.always(Math.random + 2000)
val later = Eval.later(Math.random + 3000)

now.value
always.value
later.value

val x = Eval.now {
  println("Computing X")
  math.random()
}

// first access
x.value
// second access
x.value

val y = Eval.always {
  println("Computing Y")
  math.random()
}

y.value
y.value

val z = Eval.later {
  println("Computing Z")
  math.random()
}

z.value
z.value

// Eval as a monad
val greeting = Eval
  .always { println("Step 1"); "Hello" }
  .map { str => println("Step 2"); s"$str world" }

greeting.value

val ans = for {
  a <- Eval.now { println("Calculating A"); 42 }
  b <- Eval.always { println("Calculating B"); 2 }
} yield {
  println("Calculating a + b")
  a + b
}

ans.value
ans.value

val saying = Eval
  .always { println("Step 1"); "The cat" }
  .map { str => println("Step 2"); s"$str sat on" }
  .memoize
  .map { str => println("Step 3"); s"$str the mat" }

saying.value
saying.value

// def factorial(n: BigInt): BigInt =
//     if(n == 1) 1 else n * factorial(n - 1)
// factorial(50000) Stack overflows

// rewritten using Eval
def factorial(n: BigInt): Eval[BigInt] =
  if (n == 1) Eval.now(1) else factorial(n - 1).map(_ * n)

// will still Stack overflow
factorial(100).value

def betterFactorial(n: BigInt): Eval[BigInt] =
  if (n == 1) { Eval.now(n) }
  else {
    Eval.defer(betterFactorial(n - 1).map(_ * n))
  }
  
betterFactorial(50000).value
