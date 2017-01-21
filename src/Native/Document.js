var _fossar$almafoss$Native_Document = function() {

	function title(title) {
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			document.title = title;
			callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
		});
	}

	return {
		title: title,
	};

}();
