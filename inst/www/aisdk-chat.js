(function() {
  "use strict";

  var controllers = {};

  function escapeHtml(value) {
    return String(value == null ? "" : value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function setHtml(el, html) {
    if (el) {
      el.innerHTML = html || "";
    }
  }

  function appendHtml(parent, html) {
    if (!parent) {
      return null;
    }
    var template = document.createElement("template");
    template.innerHTML = html || "";
    var first = template.content.firstElementChild;
    parent.appendChild(template.content);
    return first;
  }

  function controllerFor(root) {
    var chatId = root.getAttribute("data-chat-id");
    if (!chatId) {
      return null;
    }
    if (!controllers[chatId]) {
      controllers[chatId] = new AisdkChatController(root, chatId);
    }
    return controllers[chatId];
  }

  function AisdkChatController(root, chatId) {
    this.root = root;
    this.chatId = chatId;
    this.messages = root.querySelector(".aisdk-chat-messages");
    this.turns = {};
    this.renderedSeq = {};
    this.currentTurn = null;
    this.pinned = true;
    this.bindInput();
    this.bindScroll();
    this.bindMedia();
    this.bindDetails();
  }

  AisdkChatController.prototype.bindInput = function() {
    var input = this.root.querySelector("textarea");
    var send = this.root.querySelector(".aisdk-chat-send");
    if (!input || !send || input.getAttribute("data-aisdk-bound") === "true") {
      return;
    }
    input.setAttribute("data-aisdk-bound", "true");
    input.addEventListener("keydown", function(event) {
      if (event.key === "Enter" && !event.shiftKey) {
        event.preventDefault();
        send.click();
      }
    });
  };

  AisdkChatController.prototype.bindScroll = function() {
    var self = this;
    if (!this.messages || this.scrollBound) {
      return;
    }
    this.scrollBound = true;
    this.messages.addEventListener("scroll", function() {
      self.pinned = self.isPinnedToBottom();
      self.updateJumpButton();
    });
    if (window.ResizeObserver) {
      this.resizeObserver = new ResizeObserver(function() {
        if (self.pinned) {
          self.scroll();
        }
      });
      this.resizeObserver.observe(this.messages);
    }
    var jump = this.root.querySelector(".aisdk-jump-bottom");
    if (jump) {
      jump.addEventListener("click", function() {
        self.pinned = true;
        self.scroll(true);
      });
    }
  };

  AisdkChatController.prototype.bindMedia = function() {
    var self = this;
    if (!this.messages || this.mediaBound) {
      return;
    }
    this.mediaBound = true;
    this.messages.addEventListener("click", function(event) {
      var button = event.target.closest && event.target.closest(".aisdk-media-thumb");
      if (!button) {
        return;
      }
      self.openMedia(button.getAttribute("data-src") || "");
    });
    this.messages.addEventListener("load", function(event) {
      if (event.target && event.target.tagName === "IMG" && self.pinned) {
        self.scroll();
      }
    }, true);
  };

  AisdkChatController.prototype.bindDetails = function() {
    var self = this;
    if (!this.messages || this.detailsBound) {
      return;
    }
    this.detailsBound = true;
    this.messages.addEventListener("toggle", function(event) {
      if (event.target && event.target.classList && event.target.classList.contains("aisdk-thinking")) {
        if (self.pinned) {
          self.scroll();
        }
      }
    }, true);
  };

  AisdkChatController.prototype.isPinnedToBottom = function() {
    if (!this.messages) {
      return true;
    }
    return this.messages.scrollHeight - this.messages.scrollTop - this.messages.clientHeight < 24;
  };

  AisdkChatController.prototype.updateJumpButton = function() {
    var jump = this.root.querySelector(".aisdk-jump-bottom");
    if (jump) {
      jump.style.display = this.pinned ? "none" : "inline-flex";
    }
  };

  AisdkChatController.prototype.scroll = function(force) {
    if (this.messages && (force || this.pinned)) {
      this.messages.scrollTop = this.messages.scrollHeight;
    }
    this.updateJumpButton();
  };

  AisdkChatController.prototype.setBusy = function(value) {
    this.root.setAttribute("data-busy", value ? "true" : "false");
    this.scroll();
  };

  AisdkChatController.prototype.userMessage = function(message) {
    if (!this.messages) {
      return;
    }
    var body = message.blocks ? this.renderBlocks(message.blocks, "user") : (message.html || escapeHtml(message.content || ""));
    appendHtml(
      this.messages,
      '<article class="aisdk-message user" data-turn-id="' +
        escapeHtml(message.turn_id || "") + '">' +
        body +
      "</article>"
    );
    this.scroll();
  };

  AisdkChatController.prototype.assistantStart = function(message) {
    if (!this.messages || !message.turn_id) {
      return null;
    }
    var html = [
      '<article class="aisdk-turn assistant" data-turn-id="',
      escapeHtml(message.turn_id),
      '" data-status="',
      escapeHtml(message.status || "thinking"),
      '">',
      '<header class="aisdk-turn-header">',
      '<span class="aisdk-turn-title">Assistant</span>',
      '<span class="aisdk-turn-status">',
      escapeHtml(message.status || "thinking"),
      '</span>',
      '</header>',
      '<div class="aisdk-turn-body">',
      '<div class="aisdk-turn-blocks"></div>',
      '</div>',
      '</article>'
    ].join("");
    var el = appendHtml(this.messages, html);
    var turn = {
      id: message.turn_id,
      el: el,
      status: el ? el.querySelector(".aisdk-turn-status") : null,
      blocks: el ? el.querySelector(".aisdk-turn-blocks") : null,
      thinking: el ? el.querySelector(".aisdk-turn-blocks") : null,
      tools: el ? el.querySelector(".aisdk-turn-blocks") : null,
      content: el ? el.querySelector(".aisdk-turn-blocks") : null,
      toolMap: {}
    };
    this.turns[message.turn_id] = turn;
    this.currentTurn = turn;
    this.scroll();
    return turn;
  };

  AisdkChatController.prototype.getTurn = function(turnId) {
    if (turnId && this.turns[turnId]) {
      return this.turns[turnId];
    }
    return this.currentTurn;
  };

  AisdkChatController.prototype.turnFromElement = function(el) {
    if (!el) {
      return null;
    }
    var turnId = el.getAttribute("data-turn-id");
    if (turnId && this.turns[turnId]) {
      return this.turns[turnId];
    }
    var turn = {
      id: turnId || "",
      el: el,
      status: el.querySelector(".aisdk-turn-status"),
      blocks: el.querySelector(".aisdk-turn-blocks"),
      thinking: el.querySelector(".aisdk-turn-blocks"),
      tools: el.querySelector(".aisdk-turn-blocks"),
      content: el.querySelector(".aisdk-turn-blocks"),
      toolMap: {}
    };
    if (turnId) {
      this.turns[turnId] = turn;
    }
    return turn;
  };

  AisdkChatController.prototype.renderBlocks = function(blocks, role) {
    var self = this;
    return (blocks || []).map(function(block) {
      var type = block.type || "markdown";
      if (type === "thinking") {
        return '<details class="aisdk-thinking" ' + (block.collapsed ? "" : "open") + ' data-block-id="' + escapeHtml(block.block_id || "") + '">' +
          '<summary>Thinking</summary><div class="aisdk-thinking-body">' + (block.html || "") + '</div></details>';
      }
      if (type === "tool") {
        return self.renderToolBlock(block);
      }
      if (type === "media") {
        return '<div class="aisdk-media-block" data-block-id="' + escapeHtml(block.block_id || "") + '">' + (block.html || "") + '</div>';
      }
      if (type === "error") {
        return block.html || ('<div class="aisdk-error">' + escapeHtml(block.content || "Error") + '</div>');
      }
      return '<div class="aisdk-markdown-block ' + escapeHtml(role || "") + '" data-block-id="' + escapeHtml(block.block_id || "") + '">' + (block.html || "") + '</div>';
    }).join("");
  };

  AisdkChatController.prototype.renderToolBlock = function(block) {
    var open = block.open ? " open" : "";
    var body = block.html || "";
    if (!body && block.debug) {
      body = '<div class="aisdk-tool-label">Input arguments</div><pre>' + escapeHtml(block.arguments || "{}") + '</pre>';
    }
    return [
      '<details class="aisdk-tool" data-tool-id="', escapeHtml(block.block_id || ""), '" data-status="', escapeHtml(block.status || "running"), '"', open, '>',
      '<summary><span class="aisdk-tool-name">Tool: ', escapeHtml(block.name || "tool"), '</span>',
      '<span class="aisdk-tool-status">', escapeHtml(block.status || "running"), '</span></summary>',
      '<div class="aisdk-tool-body">', body, '</div></details>'
    ].join("");
  };

  AisdkChatController.prototype.applyTurnSnapshot = function(message, terminal) {
    if (!message || !message.turn_id) {
      return;
    }
    var seq = Number(message.seq || 0);
    if (this.renderedSeq[message.turn_id] && seq < this.renderedSeq[message.turn_id]) {
      return;
    }
    this.renderedSeq[message.turn_id] = seq;
    var turn = this.getTurn(message.turn_id) || this.assistantStart(message);
    if (!turn) {
      return;
    }
    if (message.model && message.model.label) {
      var title = turn.el.querySelector(".aisdk-turn-title");
      if (title) {
        title.textContent = "Assistant · " + message.model.label;
      }
    }
    setHtml(turn.blocks || turn.content, this.renderBlocks(message.blocks || [], "assistant"));
    this.setStatus(turn, message.status || (terminal ? "done" : "answering"));
    if (terminal && this.currentTurn === turn) {
      this.currentTurn = null;
    }
    this.scroll();
  };

  AisdkChatController.prototype.openMedia = function(src) {
    if (!src) {
      return;
    }
    var modal = document.createElement("div");
    modal.className = "aisdk-media-modal";
    modal.innerHTML = '<button type="button" class="aisdk-media-close" aria-label="Close">×</button><img src="' + escapeHtml(src) + '" alt="Attached image preview">';
    modal.addEventListener("click", function(event) {
      if (event.target === modal || event.target.className === "aisdk-media-close") {
        modal.remove();
      }
    });
    document.body.appendChild(modal);
  };

  AisdkChatController.prototype.latestOpenTurn = function() {
    if (!this.messages) {
      return null;
    }
    var nodes = this.messages.querySelectorAll(".aisdk-turn.assistant");
    for (var i = nodes.length - 1; i >= 0; i--) {
      var turn = this.turnFromElement(nodes[i]);
      if (turn && !this.isTerminal(turn)) {
        return turn;
      }
    }
    return null;
  };

  AisdkChatController.prototype.setStatus = function(turn, status) {
    if (!turn || !turn.el) {
      return;
    }
    turn.el.setAttribute("data-status", status || "");
    if (turn.status) {
      turn.status.textContent = status || "";
    }
  };

  AisdkChatController.prototype.isTerminal = function(turn) {
    if (!turn || !turn.el) {
      return false;
    }
    var status = turn.el.getAttribute("data-status");
    return status === "done" || status === "error";
  };

  AisdkChatController.prototype.contentReplace = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (!turn) {
      return;
    }
    if (this.isTerminal(turn)) {
      return;
    }
    setHtml(turn.content, message.html || "");
    if (message.status) {
      this.setStatus(turn, message.status);
    }
    this.scroll();
  };

  AisdkChatController.prototype.thinkingReplace = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (!turn || !turn.thinking) {
      return;
    }
    if (this.isTerminal(turn)) {
      return;
    }
    if (!message.html) {
      setHtml(turn.thinking, "");
      return;
    }
    setHtml(
      turn.thinking,
      '<details class="aisdk-thinking" open><summary>Thinking</summary>' +
        '<div class="aisdk-thinking-body">' +
        message.html +
        "</div></details>"
    );
    this.setStatus(turn, message.status || "thinking");
    this.scroll();
  };

  AisdkChatController.prototype.toolStart = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (!turn || !turn.tools || !message.tool_id) {
      return;
    }
    if (this.isTerminal(turn)) {
      return;
    }
    var open = message.open ? " open" : "";
    var args = message.debug
      ? '<div class="aisdk-tool-label">Input arguments</div><pre>' +
          escapeHtml(message.arguments || "{}") +
        "</pre>"
      : "";
    var html = [
      '<details class="aisdk-tool" data-tool-id="',
      escapeHtml(message.tool_id),
      '" data-status="running"',
      open,
      '>',
      "<summary>",
      '<span class="aisdk-tool-name">Tool: ',
      escapeHtml(message.name || "tool"),
      "</span>",
      '<span class="aisdk-tool-status">running</span>',
      "</summary>",
      '<div class="aisdk-tool-body">',
      args,
      '<div class="aisdk-tool-result"></div>',
      "</div>",
      "</details>"
    ].join("");
    var el = appendHtml(turn.tools, html);
    turn.toolMap[message.tool_id] = el;
    this.setStatus(turn, message.status || ("tool: " + (message.name || "tool")));
    this.scroll();
  };

  AisdkChatController.prototype.toolResult = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (this.isTerminal(turn)) {
      return;
    }
    var tool = turn && turn.toolMap ? turn.toolMap[message.tool_id] : null;
    if (!tool) {
      return;
    }
    tool.setAttribute("data-status", message.success ? "done" : "error");
    var statusEl = tool.querySelector(".aisdk-tool-status");
    if (statusEl) {
      statusEl.textContent = message.success ? "done" : "error";
    }
    var resultEl = tool.querySelector(".aisdk-tool-result");
    if (resultEl) {
      resultEl.className = "aisdk-tool-result " + (message.success ? "ok" : "error");
      setHtml(resultEl, message.html || "");
    }
    if (!message.debug) {
      tool.removeAttribute("open");
    }
    if (message.status) {
      this.setStatus(turn, message.status);
    }
    this.scroll();
  };

  AisdkChatController.prototype.assistantEnd = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (!turn) {
      return;
    }
    if (message.html != null) {
      setHtml(turn.content, message.html);
    }
    this.setStatus(turn, message.status || "done");
    if (this.currentTurn === turn) {
      this.currentTurn = null;
    }
    this.scroll();
  };

  AisdkChatController.prototype.error = function(message) {
    var turn = this.getTurn(message.turn_id);
    if (turn && turn.content) {
      setHtml(
        turn.content,
        '<div class="aisdk-error">' + escapeHtml(message.message || "Error") + "</div>"
      );
      this.setStatus(turn, "error");
      if (this.currentTurn === turn) {
        this.currentTurn = null;
      }
    } else if (this.messages) {
      appendHtml(
        this.messages,
        '<article class="aisdk-message assistant aisdk-error">' +
          escapeHtml(message.message || "Error") +
        "</article>"
      );
    }
    this.setBusy(false);
    this.scroll();
  };

  AisdkChatController.prototype.handle = function(message) {
    if (!message || !message.type) {
      return;
    }
    if (message.type === "chat_init") {
      this.setBusy(!!message.busy);
    } else if (message.type === "user_message") {
      this.userMessage(message);
    } else if (message.type === "assistant_start" || message.type === "turn_start") {
      this.assistantStart(message);
      if (message.type === "turn_start") {
        this.applyTurnSnapshot(message, false);
      }
    } else if (message.type === "turn_patch") {
      this.applyTurnSnapshot(message, false);
    } else if (message.type === "turn_done") {
      this.applyTurnSnapshot(message, true);
    } else if (message.type === "turn_error") {
      this.applyTurnSnapshot(message, true);
    } else if (message.type === "content_replace") {
      this.contentReplace(message);
    } else if (message.type === "thinking_replace") {
      this.thinkingReplace(message);
    } else if (message.type === "tool_start") {
      this.toolStart(message);
    } else if (message.type === "tool_result") {
      this.toolResult(message);
    } else if (message.type === "assistant_end") {
      this.assistantEnd(message);
    } else if (message.type === "error") {
      this.error(message);
    } else if (message.type === "set_busy" || message.type === "busy_patch") {
      this.setBusy(!!message.busy);
    } else if (message.type === "model_patch") {
      this.root.setAttribute("data-model", (message.model && message.model.id) || "");
      var modelInput = this.root.querySelector("select[id$='-model'], input[id$='-model']");
      if (modelInput && message.model && message.model.id) {
        modelInput.value = message.model.id;
      }
    }
  };

  function initialize() {
    var roots = document.querySelectorAll(".aisdk-chat-root");
    for (var i = 0; i < roots.length; i++) {
      var controller = controllerFor(roots[i]);
      if (!controller) {
        continue;
      }
      var handlerName = "aisdk_chat_event_" + controller.chatId;
      if (window.Shiny && !controller.handlerRegistered) {
        controller.handlerRegistered = true;
        window.Shiny.addCustomMessageHandler(handlerName, function(message) {
          var chatId = message && message.chat_id;
          var target = chatId ? controllers[chatId] : controller;
          if (target) {
            target.handle(message);
          }
        });
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize);
  } else {
    initialize();
  }

  document.addEventListener("shiny:connected", initialize);
  document.addEventListener("shiny:bound", initialize);
})();
