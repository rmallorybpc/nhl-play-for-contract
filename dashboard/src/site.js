(function () {
  const btn = document.getElementById("tmgDdBtn");
  const menu = document.getElementById("tmgDdMenu");

  if (btn && menu) {
    btn.addEventListener("click", function (event) {
      event.stopPropagation();
      const isOpen = menu.classList.toggle("open");
      btn.classList.toggle("open", isOpen);
      btn.setAttribute("aria-expanded", String(isOpen));
    });

    document.addEventListener("click", function () {
      menu.classList.remove("open");
      btn.classList.remove("open");
      btn.setAttribute("aria-expanded", "false");
    });

    document.addEventListener("keydown", function (event) {
      if (event.key === "Escape") {
        menu.classList.remove("open");
        btn.classList.remove("open");
        btn.setAttribute("aria-expanded", "false");
        btn.focus();
      }
    });
  }

  const yearNodes = document.querySelectorAll(".js-year");
  const yearText = String(new Date().getFullYear());
  yearNodes.forEach(function (node) {
    node.textContent = yearText;
  });

  const resultsNode = document.getElementById("explorer-results-content");
  if (!resultsNode) {
    return;
  }

  const tierNode = document.getElementById("filter-tier");
  const trajectoryNode = document.getElementById("filter-trajectory");
  const retentionNode = document.getElementById("filter-retention");

  function renderPlaceholder(data) {
    const tier = tierNode ? tierNode.value : "all";
    const trajectory = trajectoryNode ? trajectoryNode.value : "all";
    const retention = retentionNode ? retentionNode.value : "all";

    const lines = [
      "Selected filters:",
      "Tier: " + tier,
      "Trajectory: " + trajectory,
      "Retention status: " + retention,
      "",
      data.message,
      "",
      "[PHASE 5 FINDING: explorer aggregate cards and table values]"
    ];

    resultsNode.textContent = lines.join("\n");
  }

  function attachFilterHandlers(data) {
    [tierNode, trajectoryNode, retentionNode].forEach(function (node) {
      if (!node) {
        return;
      }
      node.addEventListener("change", function () {
        renderPlaceholder(data);
      });
    });
  }

  fetch("./data/phase5-findings-placeholder.json")
    .then(function (response) {
      if (!response.ok) {
        throw new Error("Failed to load placeholder data");
      }
      return response.json();
    })
    .then(function (data) {
      renderPlaceholder(data);
      attachFilterHandlers(data);
    })
    .catch(function () {
      resultsNode.textContent = "Data placeholder file is missing. Add dashboard/src/data/phase5-findings-placeholder.json.";
    });
})();
