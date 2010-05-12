package engine
{
	public class TestPlayer extends Player
	{
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		private var _move_counter:int = 0;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		public function TestPlayer(color:int)
		{
			// call the parent constructor to remove abstract
			super(this, color);
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		public override function make_move():int
		{
			var next_move:int = this._move_counter;
			if (++this._move_counter == 4) this._move_counter = 0;
			return next_move;
		}
	}
}