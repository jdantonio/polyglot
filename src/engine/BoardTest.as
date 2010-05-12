package engine
{
	import flexunit.framework.TestCase;
	
	public class BoardTest extends TestCase
	{
		public function testConstructionWithoutArgs():void
		{
			var board:Board = new Board();
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.win_condition, 4);
		}
		
		public function testConstructionWithValidArgs():void
		{
			var board:Board = new Board(10, 7, 5);
			
			assertEquals(board.width, 10);
			assertEquals(board.height, 7);
			assertEquals(board.win_condition, 5);
		}
		
		public function testConstructionWithInvalidArgs():void
		{
			var board:Board = new Board(-1, 0, 1000);
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.win_condition, 4);
		}
		
		public function testNumOfWinPlaces():void
		{
			// All of these tests values were manually counted
			// on a lined grid in Numbers. This isn't the best
			// way to create a test set but I could not find
			// a good reference.
			
			// This algorithm has four equivalence classes.
			// Each test below is for one of those classes.
			
			// invalid board values
			assertEquals(Board.num_of_win_places(5, 5, 10), 0);
			
			// width is less than number to connect
			assertEquals(Board.num_of_win_places(3, 6, 4), 9);
			
			// height is less than number to connect
			assertEquals(Board.num_of_win_places(6, 4, 5), 8);
			
			// standard game
			assertEquals(Board.num_of_win_places(7, 6, 4), 69);
			assertEquals((new Board(7, 6, 4)).num_of_win_places, 69);
		}
		
		public function testDropPieceVertical():void
		{
			// valid vertical drops followed by column full
			var board:Board = new Board();
			for (var i:int = 0; i < board.height; i++)
			{
				assertEquals(board.drop_piece(Player.BLACK, 0), i);
				assertEquals(board.drop_piece(Player.RED, 1), i);
			}
			assertEquals(board.drop_piece(Player.BLACK, 0), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(Player.RED, 1), Board.INVALID_MOVE);
		}
		
		public function testDropPieceHorizontal():void
		{
			// valid horizontal drops
			var board:Board = new Board();
			for (var i:int = 0; i < board.width; i++)
			{
				assertEquals(board.drop_piece(Player.BLACK, i), 0);
				assertEquals(board.drop_piece(Player.RED, i), 1);
			}
		}
		
		public function testDropPieceInvalidColumn():void
		{
			// invalid colums
			var board:Board = new Board();
			assertEquals(board.drop_piece(Player.RED, -1), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(Player.BLACK, board.width), Board.INVALID_MOVE);
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
