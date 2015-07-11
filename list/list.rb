class List

  include Enumerable

  def reverse
    inject(Nil.new) { |tail, head| Cons.new(head, tail) }
  end

  def [](n)
    inject([nil, n]) { |acc, value| if acc[1] <= 0 then [acc[0] || value, acc[1] - 1] else [acc[0], acc[1] - 1] end }[0]
  end

  def self.create(*values)
    values.inject(->(k) { k.call(Nil.new) }) { |acc, head|
      ->(k) {
        acc.call(->(tail) { Cons.new(head, k.call(tail)) })
      }
    }.call(->(x) { x })
  end

end

class Nil < List

  def +(list)
    list
  end

  def ==(list)
    list.is_a?(Nil)
  end

  def each(&block)
  end

end

class Cons < List

  attr_reader :head, :tail

  def initialize(head, tail)
    @head = head
    @tail = tail
  end

  def +(list)
    Cons.new(@head, @tail + list)
  end

  def ==(list)
    list.is_a?(Cons) && @head == list.head && @tail == list.tail
  end

  def each(&block)
    block.call(@head)
    @tail.each { |x| block.call(x) }
  end

end

if __FILE__ == $0
  require 'test/unit/assertions'
  include Test::Unit::Assertions
  hoge = List.create(0, 1, 2)
  assert(hoge.count == 3)
  assert(hoge[1] == 1)
  assert(hoge[2] == 2)
  assert(hoge[3] == nil)
  assert(hoge + hoge == List.create(0, 1, 2, 0, 1, 2))
  assert(hoge.reverse() == List.create(2, 1, 0))
end
