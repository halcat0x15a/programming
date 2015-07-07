module List

  def self.create(*values)
    values.inject(->(k) { k.call(Nil.new) }) { |acc, head|
      ->(k) {
        acc.call(->(tail) { Cons.new(head, k.call(tail)) })
      }
    }.call(->(x) { x })
  end

  class Nil

    def [](n)
      nil
    end

    def size
      0
    end

  end

  class Cons

    def initialize(head, tail)
      @head = head
      @tail = tail
    end

    def [](n)
      if n == 0
        @head
      else
        @tail[n - 1]
      end
    end

    def size
      1 + @tail.size
    end

  end

end

if __FILE__ == $0
  require 'test/unit/assertions'
  include Test::Unit::Assertions
  hoge = List.create(0, 1, 2)
  assert(hoge.size == 3)
  assert(hoge[1] == 1)
  assert(hoge[2] == 2)
  assert(hoge[3] == nil)
end
