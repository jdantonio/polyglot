package engine
{
	/**
	 * A simple Player subclass used only for unit testing. It cannot be used
	 * for playing actual games.
	 */
	public class TestPlayer extends Player
	{
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		private var _move_counter:int = 0;
		
		private var _reset:int;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		/**
		 * @param board The board on which this player is playing.
		 * @param color The color of playing piece this player is using.
		 * @param reset The number of calls to this method after which the
		 *        return value will be reset to zero.
		 */
		public function TestPlayer(board:Board, color:int, reset:int = 4)
		{
			// call the parent constructor to remove abstract
			super(this, board, color);
			
			this._reset = reset;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		/**
		 * Follow a simple algorithm for selecting the next move. The first
		 * time his method is called it will return zero. On each subsequent
		 * call it will increment the return value by one. After a number of
		 * calls equal to the reset value set at construction the counter
		 * will reset and the next call will again return zero.
		 * 
		 * @return The column of the next move.
		 */
		public override function make_move():int
		{
			var next_move:int = this._move_counter;
			if (++this._move_counter == this._reset) this._move_counter = 0;
			return next_move;
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
