"""Block YT ads."""


from qutebrowser.api import interceptor


def filter_yt(info: interceptor.Request):
	"""Block the given request if necessary."""
	url = info.request_url
	if (url.host() == 'www.youtube.com' and
			url.path() == '/get_video_info' and
			'&adformat=' in url.query()):
		info.block()


interceptor.register(filter_yt)
