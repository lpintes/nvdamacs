"""JSON-RPC client for communication with Emacs eval server."""

import json
import socket
import struct
import threading
import time


class RpcError(Exception):
	"""Exception raised when an RPC call returns an error."""

	def __init__(self, error):
		self.code = error.get("code")
		self.message = error.get("message")
		self.data = error.get("data")
		super().__init__(f"RPC Error {self.code}: {self.message}")


class RpcWorker(threading.Thread):
	"""Background thread that handles socket I/O for JSON-RPC communication."""

	def __init__(self, host, port):
		super().__init__(daemon=True)
		self.host = host
		self.port = port
		self.socket = None
		self.running = False
		self.lock = threading.Lock()
		self.pendingRequests = {}  # id -> Event
		self.responses = {}  # id -> response dict
		self.eventHandlers = {}  # event -> callback
		self.nextId = 1

	def connect(self):
		"""Establish socket connection to Emacs server."""
		self.socket = socket.create_connection((self.host, self.port), timeout=10)

	def run(self):
		"""Main socket I/O loop - reads messages and routes them."""
		while self.running:
			try:
				# Read 4-byte length prefix
				rawLen = self._recvExact(4)
				if not rawLen:
					break

				(msgLen,) = struct.unpack(">I", rawLen)

				# Read JSON payload
				payload = self._recvExact(msgLen)
				if not payload:
					break

				# Parse and route message
				jsonStr = payload.decode('utf-8')
				msg = json.loads(jsonStr)
				self._routeMessage(msg)

			except (ConnectionError, socket.timeout) as e:
				print(f"RpcWorker connection error: {e}")
				break
			except json.JSONDecodeError as e:
				print(f"RpcWorker JSON parse error: {e}")
				continue
			except Exception as e:
				print(f"RpcWorker unexpected error: {e}")
				break

	def _recvExact(self, n):
		"""Receive exactly n bytes from socket."""
		data = b""
		while len(data) < n:
			packet = self.socket.recv(n - len(data))
			if not packet:
				return None
			data += packet
		return data

	def _routeMessage(self, msg):
		"""Route message to appropriate handler based on type."""
		msgType = msg.get("type")

		if msgType == "response":
			self._handleResponse(msg)
		elif msgType == "event":
			self._handleEvent(msg)
		else:
			print(f"Unknown message type: {msgType}")

	def _handleResponse(self, msg):
		"""Handle response message by signaling waiting thread."""
		requestId = msg.get("id")
		if requestId is None:
			return

		with self.lock:
			self.responses[requestId] = msg
			if requestId in self.pendingRequests:
				self.pendingRequests[requestId].set()

	def _handleEvent(self, msg):
		"""Handle event message by calling registered handler on main thread."""
		event = msg.get("event")
		if event is None:
			return

		handler = self.eventHandlers.get(event)
		if handler:
			# Marshal to main thread using wx.CallAfter
			try:
				import wx
				wx.CallAfter(handler, msg.get("data", {}))
			except Exception as e:
				print(f"Error calling event handler for {event}: {e}")

	def sendMessage(self, msg):
		"""Send JSON message to Emacs."""
		jsonStr = json.dumps(msg)
		payload = jsonStr.encode('utf-8')
		length = struct.pack(">I", len(payload))
		self.socket.sendall(length + payload)

	def getNextId(self):
		"""Get next request ID in a thread-safe manner."""
		with self.lock:
			requestId = self.nextId
			self.nextId += 1
			return requestId

	def stop(self):
		"""Stop the worker thread."""
		self.running = False
		if self.socket:
			try:
				self.socket.close()
			except:
				pass


class RpcClient:
	"""Main thread API for making JSON-RPC requests to Emacs."""

	def __init__(self, portFile):
		self.worker = None
		self.portFile = portFile

	def connect(self):
		"""Connect to Emacs server and start worker thread."""
		port = self._readPortFile()
		if port is None:
			return False

		self.worker = RpcWorker("127.0.0.1", port)
		try:
			self.worker.connect()
			self.worker.running = True
			self.worker.start()
			return True
		except Exception as e:
			print(f"Failed to connect to Emacs server: {e}")
			return False

	def _readPortFile(self):
		"""Read port number from port file."""
		try:
			with open(self.portFile, 'r', encoding='utf-8') as f:
				portStr = f.read().strip()
			return int(portStr)
		except (FileNotFoundError, ValueError) as e:
			print(f"Failed to read port file: {e}")
			return None

	def request(self, method, params=None, timeout=2.0):
		"""Make blocking request and wait for response."""
		if not self.worker or not self.worker.running:
			raise RuntimeError("RpcClient not connected")

		requestId = self.worker.getNextId()
		event = threading.Event()

		with self.worker.lock:
			self.worker.pendingRequests[requestId] = event

		msg = {"type": "request", "id": requestId, "method": method}
		if params:
			msg["params"] = params

		try:
			self.worker.sendMessage(msg)
		except Exception as e:
			with self.worker.lock:
				self.worker.pendingRequests.pop(requestId, None)
			raise RuntimeError(f"Failed to send request: {e}")

		if not event.wait(timeout):
			with self.worker.lock:
				self.worker.pendingRequests.pop(requestId, None)
			raise TimeoutError(f"Request {requestId} (method: {method}) timed out after {timeout}s")

		with self.worker.lock:
			response = self.worker.responses.pop(requestId)
			self.worker.pendingRequests.pop(requestId, None)

		if "error" in response:
			raise RpcError(response["error"])
		return response.get("result")

	def registerEventHandler(self, event, callback):
		"""Register callback for event type."""
		if self.worker:
			self.worker.eventHandlers[event] = callback

	def close(self):
		"""Close connection and stop worker thread."""
		if self.worker:
			self.worker.stop()
			self.worker = None
