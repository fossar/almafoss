var _fossar$almafoss$Native_Window = function() {

	function open(url) {
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var win = window.open(url);
			if (win === null) {
				callback(_elm_lang$core$Native_Scheduler.fail({
					ctor: 'CannotOpen'
				}));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
		});
	}

	return {
		open: open,
	};

}();
