package engine
{
	import flexunit.framework.TestCase;
	
	public class GameIntegrationTest extends TestCase
	{
		public function testSimpleGame():void
		{
			return;
			
			// create the first player
			var player1:TestPlayer = new TestPlayer(Player.BLACK);			
			
			// create the second player
			var player2:TestPlayer = new TestPlayer(Player.RED);			
						
			// create a new game
			var board:Board = new Board();
			
			// set active player
			var active_player:Player = player1;
			
			// while no winner and no tie
			while (board.winner == Player.NONE)
			{
				// active player moves
				var next_move:int = active_player.make_move();
				var next_color:int = active_player.color;
				var row:int = board.drop_piece(next_color, next_move);

				// render the board
				
				// switch active player
				if (active_player == player1) {
					active_player = player2;
				} else {
					active_player = player1;
				}
			}
			
			// announce the winner
		}
	}
}