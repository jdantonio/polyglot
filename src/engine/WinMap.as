package engine
{
	/**
	 * Every combination of board width, height, and number of contiguous game
	 * pieces needed to win generates a different set of win conditions. Each
	 * win condition (possible set of n contiguous pieces) is assigned a
	 * unique number called a win index. The number assigned to each win index
	 * is irrelevant so long as each is unique. During a game the engine will
	 * check the current board state against the possible win conditions to
	 * determine if the game has been one. This class generates all possible
	 * win conditions for a given set of game parameters and stores the data.
	 * Internally the win indicies are stored in an x by y map representing
	 * the game board. Each element of the map contains a set of all win
	 * indicies using that board location.
	 */
	internal class WinMap
	{
		///////////////////////////////////////////////////////////////////////
		// Data Members

		/** The internal data map of the board and all win indicies. */
		private var _map:Array = null;
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		/**
		 * Create the map and populate it with all win indicies. Any size map
		 * with any number required to win is supported.
		 * 
		 * NOTE: This class does not validate the inputs to determine if the
		 * board is valid. It is entirely possible to create an unwinnable
		 * board. It is the responsibility of the calling code to verify
		 * the parameters before creation. If the board is not valid or
		 * winnable the array will either be null or it will be populated
		 * with empty vectors.
		 * 
		 * @param width The number of columns on the board.
		 * @param height The number of rows on the board.
		 * @param win_condition The number of contiguous board spaces
		 *   a player needs to connect in order to win the game.
		 */
		public function WinMap(width:int, height:int, win_condition:int)
		{
			// check for positive dimensions
			if (width <= 0 || height <= 0) return;
			
			// loop control variables
			var i:int, j:int, k:int;
			
			// create the map array
			this._map = new Array(width);
			for (i = 0; i < width; i++)
			{
				this._map[i] = new Array(height);
				for (j = 0; j < height; j++)
				{
					this._map[i][j] = new Vector.<int>;
				}
			}
			
			// initialize the win index
			// this value will increment each time the map is marked
			var win_index:int = 0;
			
			// fill in the horizontal win positions
			for (i = 0; i < height; i++)
			{
				// loop through all columns that can be the start of a win
				for (j = 0; j < width - win_condition + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < win_condition; k++)
					{						
						// add the new win index to the vector
						this._map[j+k][i].push(win_index);
					}
					// increment the win index
					win_index++;
				}
			}
			
			// fill in the vertical win positions
			for (i = 0; i < width; i++)
			{
				// loop through all columns that can be the start of a win
				for (j = 0; j < height - win_condition + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < win_condition; k++)
					{
						// add the new win index to the vector
						this._map[i][j+k].push(win_index);
					}
					// increment the win index
					win_index++;
				}
			}
			
			// fill in the forward diaginal win positions
			for (i = 0; i < height - win_condition + 1; i++)
			{
				for (j = 0; j < width - win_condition + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < win_condition; k++)
					{
						// add the new win index to the vector
						this._map[j+k][i+k].push(win_index);
					}					
					// increment the win index
					win_index++;
				}
			}
			
			// fill in the backward diagonal win positions
			for (i = 0; i < height - win_condition + 1; i++)
			{
				for (j = width - 1; j >= win_condition - 1; j--)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < win_condition; k++)
					{
						// add the new win index to the vector
						this._map[j-k][i+k].push(win_index);
					}
					// increment the win index
					win_index++;
				}
			}
		}
		
		/**
		 * Return all the win indicies for a given board position.
		 * This method is provided mainly to facilitate unit testing.
		 * 
		 * @param x The x-coordinate of the board position to check.
		 * @param y The y-coordinate of the board position to check.
		 * 
		 * @return A new vector containing all the win indicies for the 
		 *   requested board position. Invalid coordinates will result
		 *   in an empty vector being returned.
		 */
		public function win_indicies(x:int, y:int):Vector.<int>
		{
			var indicies:Vector.<int> = new Vector.<int>;
			
			if (this._map != null && x < this._map.length && y < this._map[0].length)
			{
				for each (var x:int in this._map[x][y]) indicies.push(x);
			}
			
			return indicies;
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
