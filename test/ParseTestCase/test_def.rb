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

  def d(a = 1, 
    b = 2, c = 3)
    i
  end

  def e(a = 2, c = 1, d, *p, &b)
  end

  def e(a = 1) $1 end
     
  def a?; i end
  def a!
    i
  end
  def a= t
    @a = t
  end
end