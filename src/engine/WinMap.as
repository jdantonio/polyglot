package engine
{
	public class WinMap
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
		
		private static const EOL:int = -1;
		
		///////////////////////////////////////////////////////////////////////
		// Data Members

		private var _map:Array;
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		public function WinMap(width:int, height:int, num_to_connect:int)
		{
			// loop control variables
			var i:int, j:int, k:int, x:int, y:int;
			
			// create the map array
			this._map = new Array(width);
			for (i = 0; i < width; i++)
			{
				this._map[i] = new Array(height);
				for (j = 0; j < height; j++)
				{
					this._map[i][j] = new Array((num_to_connect * 4) + 1);
					this._map[i][j][0] = EOL;
				}
			}
			
			// initialize the win index
			// this value will increment each time the map is marked
			var win_index:int = 0;
			
			// fill in the horizontal win positions
			for (i = 0; i < height; i++)
			{
				// loop through all columns that can be the start of a win
				for (j = 0; j < width - num_to_connect + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < num_to_connect; k++)
					{
						// find the terminator position for the current location
						for (x = 0; this._map[j+k][i][x] != EOL; x++) { }
						
						// mark the current position and the new terminator
						this._map[j+k][i][x] = win_index;
						this._map[j+k][i][x+1] = EOL;
					}
					// increment the win index
					win_index++;
				}
			}
			
			// fill in the vertical win positions
			for (i = 0; i < width; i++)
			{
				// loop through all columns that can be the start of a win
				for (j = 0; j < height - num_to_connect + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < num_to_connect; k++)
					{
						// find the terminator position for the current location
						for (x = 0; this._map[i][j+k][x] != EOL; x++) { }
						
						// mark the current position and the new terminator
						this._map[i][j+k][x] = win_index;
						this._map[i][j+k][x+1] = EOL;
					}
					// increment the win index
					win_index++;
				}
			}
			
			// fill in the forward diaginal win positions
			// NOTE: The algorithm in the Pomakis source does not match the sample map.
			//       This algorithm was modified to match the sample data.
			for (i = 0; i < width - num_to_connect + 1; i++)
			{
				for (j = 0; j < height - num_to_connect + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < num_to_connect; k++)
					{
						// find the terminator position for the current location
						for (x = 0; this._map[i+k][j+k][x] != EOL; x++) { }
						
						// mark the current position and the new terminator
						this._map[i+k][j+k][x] = win_index;
						this._map[i+k][j+k][x+1] = EOL;
					}					
					// increment the win index
					win_index++;
				}
			}
			/*
			for (i = 0; i < height - num_to_connect + 1; i++)
			{
				for (j = 0; j < width - num_to_connect + 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < num_to_connect; k++)
					{
						// find the terminator position for the current location
						for (x = 0; this._map[j+k][i+k][x] != EOL; x++) { }
						
						// mark the current position and the new terminator
						this._map[j+k][i+k][x] = win_index;
						this._map[j+k][i+k][x+1] = EOL;
					}					
					// increment the win index
					win_index++;
				}
			}
			*/
			
			// fill in the backward diagonal win positions
			// NOTE: The algorithm in the Pomakis source does not match the sample map.
			//       This algorithm was modified to match the sample data.
			for (i = 0; i < height - num_to_connect + 1; i++)
			{
				for (j = width - 1; j < num_to_connect - 1; j++)
				{
					// mark all winning positions with the current win index
					for (k = 0; k < num_to_connect; k++)
					{
						// find the terminator position for the current location
						for (x = 0; this._map[j-k][i+k][x] != EOL; x++) { }
						
						// mark the current position and the new terminator
						this._map[j-k][i+k][x] = win_index;
						this._map[j-k][i+k][x+1] = EOL;
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
		 * @return A vector containing all the win indicies for the 
		 *   requested board position. Invalid coordinates will result
		 *   in an empty vector being returned.
		 */
		public function win_indicies(x:int, y:int):Vector.<int>
		{
			var indicies:Vector.<int> = new Vector.<int>;
			
			if (x < this._map.length && y < this._map[0].length)
			{
				for (var i:int = 0; this._map[x][y][i] != EOL; i++)
				{
					indicies.push(this._map[x][y][i]);
				}
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
