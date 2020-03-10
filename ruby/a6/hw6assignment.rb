# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece

end

class MyBoard < Board
  
end

class MyTetris < Tetris

  # Extends parent class' bindings as specified
  def key_bindings      
    # Add vanilla bindings
    super

    # Add 180 degree rotation binding
    flip = proc {
      @board.rotate_clockwise 
      @board.rotate_clockwise
    } 
    @root.bind('u', flip)
    end
end


