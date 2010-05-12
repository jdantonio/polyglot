package engine
{
	import flexunit.framework.TestCase;
	
	public class ScoresTest extends TestCase
	{
		public function testWinner():void
		{
			assertEquals(new Scores(new Board(1, 1, 1)).winner, Player.NONE)
		}
		
		public function testMagicWinNumber():void
		{
			assertEquals((new Scores(new Board(10, 10, 1)).magic_win_number), 2);
			assertEquals((new Scores(new Board(10, 10, 2)).magic_win_number), 4);
			assertEquals((new Scores(new Board(10, 10, 3)).magic_win_number), 8);
			assertEquals((new Scores(new Board(10, 10, 4)).magic_win_number), 16);
			assertEquals((new Scores(new Board(10, 10, 5)).magic_win_number), 32);
			assertEquals((new Scores(new Board(10, 10, 6)).magic_win_number), 64);
		}
		
		public function testUpdateScores():void
		{
			// create a 2x2x2 board and scores array
			var scores:Scores = new Scores(new Board(2, 2, 2));
			
			// drop pieces until we get a winner
			assertEquals(scores.update_score(Player.BLACK, 0, 0), Player.NONE);
			assertEquals(scores.update_score(Player.RED, 0, 1), Player.NONE);
			assertEquals(scores.update_score(Player.BLACK, 1, 0), Player.BLACK);
			assertEquals(scores.update_score(Player.RED, 1, 1), Player.BLACK);
			assertEquals(scores.winner, Player.BLACK);

			// create a standard board and scores array
			scores = new Scores(new Board(7, 6, 4));
			
			// drop pieces until we get a winner
			assertEquals(scores.update_score(Player.RED, 0, 0), Player.NONE);
			assertEquals(scores.update_score(Player.BLACK, 0, 1), Player.NONE);
			assertEquals(scores.update_score(Player.RED, 1, 0), Player.NONE);
			assertEquals(scores.update_score(Player.BLACK, 1, 1), Player.NONE);
			assertEquals(scores.update_score(Player.RED, 2, 0), Player.NONE);
			assertEquals(scores.update_score(Player.BLACK, 2, 1), Player.NONE);
			assertEquals(scores.update_score(Player.RED, 3, 0), Player.RED);
			assertEquals(scores.update_score(Player.BLACK, 3, 1), Player.RED);
			assertEquals(scores.winner, Player.RED);
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
