puts("hello")

i = 1
i.times() do |k|
  puts(k)
  k + 1
end

i.times(a, if i == 0 then i end, i *10) {
  |j,k,l|
  j * k * l
}

while i != 0
  i = i - 1 
end.times() do |j|
  puts(j)
end

File::new("./a.log") do |i|
  i.write("hehe")
end

i = Openssl::digest().md5("a")

1.1.times() do |i| i = 0 end