package engine
{
	/**
	 * A basic artifical intelligence for playing connect four either against
	 * a human player or another AI. The algorithms employed were developed by
	 * Keith Pomakis in 1992 and eventually released into the public domain.
	 */
	public class AIPlayer extends Player
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
	
		/** The minimum skill level that the AI can be set to. */
		public static const MIN_SKILL:int = 1;

		/** The maximum skill level that the AI can be set to. */
		public static const MAX_SKILL:int = 20;
		
		/** The default skill level for the AI when not explicitly set. */
		public static const DEFAULT_SKILL:int = 5;
		
		///////////////////////////////////////////////////////////////////////
		// Data Members

		/** The current skill level of the AI. */
		private var _skill:int;
		
		/** An ordered list of columns for the AI to check when making moves. */
		private var _drop_order:Array;
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		/**
		 * @param board The board that the AI is playing the game on.
		 * @param color The color of game piece the AI is using.
		 * @param skill The skill level of the AI when it calculates its moves.
		 */
		public function AIPlayer(board:Board, color:int, skill:int = DEFAULT_SKILL)
		{
			// call the parent constructor to remove abstract
			super(this, board, color);
			
			// set the skill level
			if (skill < MIN_SKILL) {
				this._skill = MIN_SKILL;
			} else if (skill > MAX_SKILL) {
				this._skill = MAX_SKILL;
			} else {
				this._skill = skill;
			}
			
			// calculate the preferred drop order for this board
			this._drop_order = AIPlayer.drop_order(this.board.width);
		}
	
		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		/** The current skill level of the AI. */
		public function get skill():int
		{
			return this._skill;
		}
	
		///////////////////////////////////////////////////////////////////////
		// Game Operations

		/**
		 * Examine the board and determine the next move.
		 * 
		 * @return The space selected for the next move or Board.INVALID_MOVE
		 *         if the board is full.
		 */
		public override function make_move():int
		{
			// check for center-space openning move
			if (this.allis_open() != Board.INVALID_MOVE)
			{
				return this.allis_open();
			}
			
			// for tracking the best column of all options
			var best_move:int = Board.INVALID_MOVE;
			
			// for tracking the goodness of the current and best moves
			var goodness:int = 0;
			var best_good:int = int.MIN_VALUE;
			
			// simulate a drop in every column and evaluate results
			for (var check_column:int = 0; check_column < this.board.width; check_column++)
			{
				// clone the board so we can run tests in isolation
				var clone:Board = Board.clone(this.board);
				
				// make the move
				var move:int = clone.drop_piece(this.color, check_column);
				
				// check the result of the move
				if (move == Board.INVALID_MOVE)
				{
					// the column was full so skip this move
					continue;
				}
				else if (clone.winner == this.color)
				{
					// winning move, take it
					best_move = check_column;
					break;
				}
				else
				{
					// look ahead and evaluate the move
					goodness = this.evaluate(clone, this.color, 0, int.MIN_VALUE, -best_good);
				}
				
				// pick the best move so far
				if (goodness > best_good)
				{
					// this move is clearly better
					best_good = goodness;
					best_move = check_column;
				}
				else if (goodness == best_good)
				{
					// two moves of equal goodness, flip a coin
					if (Math.round(Math.random()) == 0)
					{
						best_good = goodness;
						best_move = check_column;
					}
				}
			}
			
			return best_move;
		}
		
		/**
		 * It has been mathematically proven that the best opening move on a
		 * standard 7x6 board is the center column. This method determines if
		 * it is the opening turn for the AI and takes the center column if
		 * the board has an odd number of columns and that space is available.
		 * This method does not restrict itself to a 7x6 board so it is a
		 * little more promiscuous than Victor Allis' proof.  
		 */
		protected function allis_open():int
		{
			var column:int = Board.INVALID_MOVE;
			
			if (this.board.num_of_pieces <= 1 && this.board.width % 2 == 1)
			{
				column = (this.board.width - 1) / 2;
			}
			
			return column;
		}

		/**
		 * Recursively check moves for both the AI and the opposing player to
		 * determine which move is the most liekly to lead to victory. The
		 * greater the skill level of the AI the more potential moves forward
		 * it will evaluate. An alpha-beta pruning algorithm is used to limit
		 * the number of nodes evaluated. This method is based entirely on
		 * the logic developed by Pomakis. 
		 * 
		 * @params board The clone board being used to propose future moves.
		 * @params color The color of the player for which the board will
		 *         be evaluated.
		 * @params depth The number of levels deep the evaluation has gone.
		 * @params alpha The current player's best choice.
		 * @param beta The opposing player's best choice.
		 */
		private function evaluate(board:Board, color:int, depth:int, alpha:int, beta:int):int
		{
			var goodness:int = 0;
			
			if (board.winner == color)
			{
				// higest possible score
				goodness = int.MAX_VALUE - depth;
			}
			else if (board.is_tie())
			{
				// a tie
				goodness = 0;
			}
			else if (this._skill == depth)
			{
				// this is as far as we go
				goodness = board.score(color) - board.score(Player.other_color(color));
			}
			else
			{
				// assume its the other player's turn
				var best:int = -int.MAX_VALUE;
				var maxab:int = alpha;

				// iterate through all columns
				for (var i:int = 0; i < board.width; i++)
				{
					// skip full columns
					if (! board.is_column_open(this._drop_order[i]))
					{
						continue;
					}
					
					// recursively check the current column
					var clone:Board = Board.clone(board);
					var other_color:int = Player.other_color(color);
					clone.drop_piece(other_color, this._drop_order[i]);
					goodness = evaluate(clone, other_color, depth+1, -beta, -maxab);
					
					// evaluate the new goodness
					if (goodness > best)
					{
						best = goodness;
						if (best > maxab) maxab = best;
					}

					// decide if the best is good enough
					if (best > beta) break;
				}
				
				// a move that's good for the other player is bad for us
				goodness = -best;
			}
			
			return goodness;
		}
		
		/**
		 * A list describing the order columnd should be checked for when choosing
		 * the next move. The theory is that the columns closest to the center
		 * are the most likely to lead to a win and should be checked first.
		 */
		public static function drop_order(width:int):Array
		{
			var order:Array = new Array(width);
			
			// start with the center column
			var column:int = (width - 1) / 2;
			
			// loop through all columns
			for (var i:int = 1; i <= width; i++)
			{
				// add the current column to the order array
				order[i-1] = column;
				
				// select the next column by moving away from the center
				column += ( (i % 2 == 1) ? i : -i);
			}
			
			return order;
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
// 
// Copyright (c) 2010 Jerry D'Antonio <stumpjumper@gmail.com>.
// 
// Licensed under MIT License http://www.opensource.org/licenses/mit-license.php
// 
////////////////////////////////////////////////////////////////////////////////
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
// 
////////////////////////////////////////////////////////////////////////////////
