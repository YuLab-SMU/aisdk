(function () {
  function parseEmbeddedJson(card, role) {
    var node = card.querySelector('[data-role="' + role + '"]');
    if (!node) return null;

    try {
      return JSON.parse(node.textContent);
    } catch (error) {
      return null;
    }
  }

  function formatTrace(provenance) {
    if (!provenance || !provenance.transcript) {
      return "No embedded agent trace is available.";
    }

    var lines = [];
    var model = provenance.model || {};
    var review = provenance.review || {};
    var execution = provenance.execution || {};
    var transcript = provenance.transcript || {};

    if (model.session_id) lines.push("Session: " + model.session_id);
    if (model.id) lines.push("Model: " + model.id);
    if (review.status) lines.push("Review Status: " + review.status);
    if (execution.status) lines.push("Execution Status: " + execution.status);
    if (lines.length) lines.push("");

    if (transcript.mode === "summary" && Array.isArray(transcript.entries)) {
      if (!transcript.entries.length) {
        lines.push("No transcript entries were embedded.");
      }

      transcript.entries.forEach(function (entry, index) {
        lines.push("[" + (index + 1) + "] " + ((entry.role || "message").toUpperCase()));
        if (entry.reasoning_excerpt) {
          lines.push("Reasoning: " + entry.reasoning_excerpt);
        }
        if (entry.excerpt) {
          lines.push("Content: " + entry.excerpt);
        }
        lines.push("");
      });

      return lines.join("\n").trim();
    }

    if (transcript.mode === "full" && Array.isArray(transcript.messages)) {
      if (!transcript.messages.length) {
        lines.push("No transcript entries were embedded.");
      }

      transcript.messages.forEach(function (entry, index) {
        lines.push("[" + (index + 1) + "] " + ((entry.role || "message").toUpperCase()));
        if (entry.reasoning) {
          lines.push("Reasoning:");
          lines.push(entry.reasoning);
          lines.push("");
        }
        if (entry.content) {
          lines.push("Content:");
          lines.push(entry.content);
          lines.push("");
        }
      });

      return lines.join("\n").trim();
    }

    return JSON.stringify(provenance, null, 2);
  }

  function formatMetadata(manifest, provenance) {
    var payload = {
      runtime: manifest && manifest.runtime ? manifest.runtime : null,
      document: manifest && manifest.document ? manifest.document : null,
      review: manifest && manifest.review ? manifest.review : null,
      execution: manifest && manifest.execution ? manifest.execution : null,
      session: manifest && manifest.session ? manifest.session : null,
      code: manifest && manifest.code ? manifest.code : null,
      model: provenance && provenance.model ? provenance.model : null,
      chunk: provenance && provenance.chunk ? provenance.chunk : null
    };

    if (!payload.runtime && !payload.document && !payload.review &&
        !payload.execution && !payload.session && !payload.code &&
        !payload.model && !payload.chunk) {
      return "No embedded artifact metadata is available.";
    }

    return JSON.stringify(payload, null, 2);
  }

  function initCard(card) {
    var provenance = parseEmbeddedJson(card, "embedded-provenance");
    var manifest = parseEmbeddedJson(card, "embedded-runtime-manifest");

    var traceNode = card.querySelector('[data-role="trace-content"]');
    if (traceNode && !traceNode.textContent.trim()) {
      traceNode.textContent = formatTrace(provenance);
    }

    var inspectorNode = card.querySelector('[data-role="inspector-content"]');
    if (inspectorNode && !inspectorNode.textContent.trim()) {
      inspectorNode.textContent = formatMetadata(manifest, provenance);
    }
  }

  function initReviewCards() {
    var cards = document.querySelectorAll(".aisdk-ai-review-card");
    cards.forEach(initCard);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initReviewCards);
  } else {
    initReviewCards();
  }
})();
