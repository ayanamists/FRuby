class A
  def a
    @a * 10
  end

  def b a, b, c, *t, &d
    a + b + c
    d
  end

  def c(fun, fuck, *ay)
    ay
  end
end