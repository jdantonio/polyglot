package engine
{
	import flexunit.framework.TestCase;
	
	public class BoardTest extends TestCase
	{
		public function testConstructionWithoutArgs():void
		{
			var board:engine.Board = new engine.Board();
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.num_to_connect, 4);
			assertEquals(board.last_player, GamePieceEnum.NONE);
		}
		
		public function testConstructionWithValidArgs():void
		{
			var board:engine.Board = new engine.Board(10, 7, 5);
			
			assertEquals(board.width, 10);
			assertEquals(board.height, 7);
			assertEquals(board.num_to_connect, 5);
			assertEquals(board.last_player, GamePieceEnum.NONE);
		}
		
		public function testConstructionWithInvalidArgs():void
		{
			var board:engine.Board = new engine.Board(-1, 0, 1000);
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.num_to_connect, 4);
			assertEquals(board.last_player, GamePieceEnum.NONE);
		}
		
		public function testNumOfWinPlaces():void
		{
			// All of these tests values were manually counted
			// on a lined grid in Numbers. This isn't the best
			// way to create a test set but I could not find
			// a good reference.
			
			// this algorithm has four equivalence classes
			// each test below is for one of those classes
			
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
