# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [rotations([[0,0], [1,0], [0,1], [1,1], [2, 0]]),
                  [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]],
                  [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
                  rotations([[0,0], [0, 1], [1, 0]])] + All_Pieces

  # your enhancements here
  Cheat_Piece = [[[0,0]]]

  def self.next_piece (board)
    if board.cheat
      MyPiece.new(Cheat_Piece, board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end
end

class MyBoard < Board
  # your enhancements here
  attr_reader :cheat

  def initialize (game)
    super
    @cheat = false
    @current_block = MyPiece.next_piece(self)
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat_func
    if !game_over? and @game.is_running?
      if @score >= 100 and !@cheat
        @score -= 100
        @cheat = true
      end
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
    @cheat = false
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  # your enhancements here
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat_func})
  end
end

class MyPieceChallenge < MyPiece

end

class MyBoardChallenge < MyBoard
  def set_faster
    if !game_over? and @game.is_running?
      @score += 1
      @game.update_score
      run
    end
  end

  def remove_two
    if !game_over? and @game.is_running?
      if @score >= 0
        (@grid.size - 2..(@grid.size-1)).each{|num| row = @grid.slice(num);
          (0..(num_columns-1)).each{|index|
            if @grid[num][index]
              @grid[num][index].remove;
            end
            @grid[num][index] = nil
          }
          ((@grid.size - num + 1)..(@grid.size)).each{|num2|
            @grid[@grid.size - num2].each{|rect| rect && rect.move(0, block_size)};
            @grid[@grid.size-num2+1] = Array.new(@grid[@grid.size - num2])
          }
          @grid[0] = Array.new(num_columns);
        }
        @score -= 0
        @game.update_score
      end
    end
  end
end

class MyTetrisChallenge < MyTetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('z', proc {@board.set_faster})
    @root.bind('p', proc {@board.remove_two})
  end
end
