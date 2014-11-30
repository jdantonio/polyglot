package engine
{
	import flash.utils.Dictionary;

	/**
	 * The current score and score statistics of both players.
	 */
	internal class ScoreKeeper
	{
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		/** The board the game is being played on. ScoreKeeper and the board are tightly coupled. */
		private var _board:Board;
		
		/** Hash for storing the current score of each player. */
		private var _score:Dictionary;
		
		/** Hash for storing the current score statistics for each player. */
		private var _scores:Dictionary;
		
		/** Object for storing all the possible winning piece combinations. */
		private var _map:WinMap;
		
		/** The winner of the game based on the current board state. May be NONE. */
		private var _winner:int;
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		/**
		 * @param board The board on which these scores are calculated.
		 */
		public function ScoreKeeper(board:Board)
		{
			// set the board reference
			this._board = board;
			
			// set the winner
			this._winner = Player.NONE;
			
			// create the score hash
			this._score = new Dictionary();
			this._score[Player.BLACK] = this._board.num_of_win_places;
			this._score[Player.RED] = this._board.num_of_win_places;
			
			// create the scores hash
			this._scores = new Dictionary();
			this._scores[Player.BLACK] = new Array(this._board.num_of_win_places);
			this._scores[Player.RED] = new Array(this._board.num_of_win_places);
			for (var i:int = 0; i < this._board.num_of_win_places; i++)
			{
				this._scores[Player.BLACK][i] = 1;
				this._scores[Player.RED][i] = 1;
			}
			
			// create the win map
			this._map = new WinMap(this._board.width, this._board.height, this._board.win_condition);
		}
		
		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		/** Magic number, based on win condition, used to calculate player scores. */
		public function get magic_win_number():int
		{
			return 1 << this._board.win_condition
		}
		
		/** The winner of the game based on the current board state. May be NONE. */
		public function get winner():int
		{
			return this._winner;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		/**
		 * @return The score of the requested player at the current game state.
		 */
		public function score(player:int):int
		{
			return this._score[player];
		}

		/**
		 * Update the scores of both players based on a given move. No attempt
		 * is made to validate the legitimacy of the move. The game must
		 * ensure that only valid moves are given. If the given move wins the
		 * game then the internal winner variable will be set appropriately.
		 * If the game has already been won the winner will not change.
		 * 
		 * @pre The move must be legal.
		 * @post The current winner will be set appropriately. If the game has
		 *       already been won then a new winner will not be assigned.
		 * 
		 * @param player The player making the move.
		 * @param column The column of the current move.
		 * @param row The row of the current move.
		 * 
		 * @return The current game winner (Player) or NONE.
		 */
		public function update_score(player:int, column:int, row:int):int
		{
			// check for valid color and current winner
			if (Player.is_valid_color(player) && this._winner == Player.NONE)
			{
				// calulation variables
				var player_diff:int = 0;
				var other_diff:int = 0;
				var other:int = Player.other_color(player);
				
				// get the win indicies for the selected board location
				var win_indicies:Vector.<int> = this._map.win_indicies(column, row);
				
				// iterate over all possible win indicies
				for each (var win_index:int in win_indicies)
				{
					// check the score differential
					player_diff += this._scores[player][win_index];
					other_diff += this._scores[other][win_index];
					
					// set new values for the current win index
					this._scores[player][win_index] <<= 1;
					this._scores[other][win_index] = 0;
					
					// check for a winner
					if (this._scores[player][win_index] == this.magic_win_number)
					{
						this._winner = player;
					}
				}
				
				this._score[player] += player_diff;
				this._score[other] -= other_diff;
			}
			
			// return the current winner
			return this._winner;
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
