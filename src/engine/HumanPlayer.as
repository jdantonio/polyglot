package engine
{
	public class HumanPlayer extends Player
	{		
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		private var _next_move:int = Board.INVALID_MOVE;

		///////////////////////////////////////////////////////////////////////
		// Construction
		
		/**
		 * @param board The board on which this player is playing.
		 * @param color The color of playing piece this player is using.
		 */
		public function HumanPlayer(board:Board, color:int)
		{
			// call the parent constructor to remove abstract
			super(this, board, color);
		}

		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		public function get next_move():int
		{
			return this._next_move;
		}
		
		public function set next_move(val:int):void
		{
			if (val < 0 || val > this.board.width) {
				this._next_move = Board.INVALID_MOVE;
			} else {
				this._next_move = val;
			}
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		public override function make_move():int
		{
			return Board.INVALID_MOVE;
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
