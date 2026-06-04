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

  const extremesNode = document.getElementById("explorer-extremes-content");
  const overpayBtn = document.getElementById("extreme-overpay");
  const discountBtn = document.getElementById("extreme-discount");
  const tierNode = document.getElementById("filter-tier");
  const trajectoryNode = document.getElementById("filter-trajectory");
  const retentionNode = document.getElementById("filter-retention");

  function parseCsv(text) {
    const lines = text.trim().split(/\r?\n/);
    if (lines.length < 2) {
      return [];
    }

    function splitLine(line) {
      const out = [];
      let current = "";
      let inQuotes = false;
      for (let i = 0; i < line.length; i += 1) {
        const ch = line[i];
        if (ch === '"') {
          if (inQuotes && line[i + 1] === '"') {
            current += '"';
            i += 1;
          } else {
            inQuotes = !inQuotes;
          }
        } else if (ch === "," && !inQuotes) {
          out.push(current);
          current = "";
        } else {
          current += ch;
        }
      }
      out.push(current);
      return out;
    }

    const headers = splitLine(lines[0]);
    return lines.slice(1).map(function (line) {
      const values = splitLine(line);
      const row = {};
      headers.forEach(function (header, idx) {
        row[header] = values[idx] !== undefined ? values[idx] : "";
      });
      return row;
    });
  }

  function toNumber(value) {
    if (value === "" || value === "NA" || value === undefined || value === null) {
      return null;
    }
    const n = Number(value);
    return Number.isNaN(n) ? null : n;
  }

  function fmt(value, digits) {
    const n = toNumber(value);
    if (n === null) {
      return "NA";
    }
    return n.toFixed(digits);
  }

  function fmtInt(value) {
    const n = toNumber(value);
    if (n === null) {
      return "NA";
    }
    return n.toLocaleString("en-US", { maximumFractionDigits: 0 });
  }

  function makeTable(headers, rows) {
    const table = document.createElement("table");
    table.className = "tmg-table";

    const thead = document.createElement("thead");
    const headRow = document.createElement("tr");
    headers.forEach(function (header) {
      const th = document.createElement("th");
      th.scope = "col";
      th.textContent = header;
      headRow.appendChild(th);
    });
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = document.createElement("tbody");
    rows.forEach(function (row) {
      const tr = document.createElement("tr");
      row.forEach(function (cell) {
        const td = document.createElement("td");
        td.textContent = cell;
        tr.appendChild(td);
      });
      tbody.appendChild(tr);
    });
    table.appendChild(tbody);
    return table;
  }

  function renderEmpty(message) {
    resultsNode.innerHTML = "";
    const p = document.createElement("p");
    p.className = "tmg-note";
    p.textContent = message;
    resultsNode.appendChild(p);
  }

  function renderRetentionRows(retentionRows, selectedRetention) {
    const rows = retentionRows.filter(function (row) {
      if (row.retention_status === "difference_same_minus_new") {
        return selectedRetention === "all";
      }
      return selectedRetention === "all" || row.retention_status === selectedRetention;
    });

    if (rows.length === 0) {
      renderEmpty("No retention rows match this filter selection.");
      return;
    }

    const tableRows = rows.map(function (row) {
      return [
        row.retention_status,
        fmtInt(row.n),
        fmt(row.mean_overpay_residual, 2),
        fmt(row.median_overpay_residual, 2),
        fmt(row.sd_overpay_residual, 2)
      ];
    });

    resultsNode.innerHTML = "";
    const note = document.createElement("p");
    note.className = "tmg-note";
    note.textContent = "Retention comparison view from retention_overpay_comparison.csv.";
    resultsNode.appendChild(note);
    resultsNode.appendChild(makeTable(
      ["Retention status", "n", "Mean residual", "Median residual", "SD"],
      tableRows
    ));
  }

  function renderWalkRows(walkRows, selectedTier, selectedTrajectory) {
    const rows = walkRows.filter(function (row) {
      if (row.tier === "ALL" && row.trajectory === "ALL") {
        return selectedTier === "all" && selectedTrajectory === "all";
      }
      if (selectedTier !== "all" && row.tier !== selectedTier) {
        return false;
      }
      if (selectedTrajectory !== "all" && row.trajectory !== selectedTrajectory) {
        return false;
      }
      return row.tier !== "ALL" && row.trajectory !== "ALL";
    });

    if (rows.length === 0) {
      renderEmpty("No walk-year bucket rows match this filter selection.");
      return;
    }

    const tableRows = rows.map(function (row) {
      return [
        row.tier + " x " + row.trajectory,
        fmtInt(row.n),
        fmt(row.mean_post_signing_toi_change, 2),
        fmt(row.mean_walk_year_trend_delta, 2),
        fmt(row.share_spike_above_trend, 3)
      ];
    });

    resultsNode.innerHTML = "";
    const note = document.createElement("p");
    note.className = "tmg-note";
    note.textContent = "Walk-year bucket view from walk_year_effect_by_bucket.csv.";
    resultsNode.appendChild(note);
    resultsNode.appendChild(makeTable(
      ["Bucket", "n", "Mean post-signing TOI change", "Mean walk-year trend delta", "Spike-above-trend share"],
      tableRows
    ));
  }

  function renderExtremes(extremeRows, activeType) {
    if (!extremesNode) {
      return;
    }

    const rows = extremeRows.filter(function (row) {
      return row.extreme_type === activeType;
    }).slice(0, 15);

    extremesNode.innerHTML = "";
    if (rows.length === 0) {
      const note = document.createElement("p");
      note.className = "tmg-note";
      note.textContent = "No rows available for this extremes view.";
      extremesNode.appendChild(note);
      return;
    }

    const tableRows = rows.map(function (row) {
      return [
        String(row.rank),
        row.player_name,
        row.signing_year,
        fmt(row.overpay_residual, 2)
      ];
    });

    extremesNode.appendChild(makeTable(
      ["Rank", "Player", "Signing year", "Residual"],
      tableRows
    ));

    if (overpayBtn && discountBtn) {
      const isOverpay = activeType === "overpay";
      overpayBtn.setAttribute("aria-pressed", String(isOverpay));
      discountBtn.setAttribute("aria-pressed", String(!isOverpay));
    }
  }

  function attachFilterHandlers(walkRows, retentionRows) {
    [tierNode, trajectoryNode, retentionNode].forEach(function (node) {
      if (!node) {
        return;
      }
      node.addEventListener("change", function () {
        const selectedTier = tierNode ? tierNode.value : "all";
        const selectedTrajectory = trajectoryNode ? trajectoryNode.value : "all";
        const selectedRetention = retentionNode ? retentionNode.value : "all";

        if (selectedRetention !== "all") {
          renderRetentionRows(retentionRows, selectedRetention);
          return;
        }

        renderWalkRows(walkRows, selectedTier, selectedTrajectory);
      });
    });
  }

  Promise.all([
    fetch("./data/walk_year_effect_by_bucket.csv").then(function (response) {
      if (!response.ok) {
        throw new Error("Failed to load walk_year_effect_by_bucket.csv");
      }
      return response.text();
    }),
    fetch("./data/retention_overpay_comparison.csv").then(function (response) {
      if (!response.ok) {
        throw new Error("Failed to load retention_overpay_comparison.csv");
      }
      return response.text();
    }),
    fetch("./data/overpay_extremes_material.csv").then(function (response) {
      if (!response.ok) {
        throw new Error("Failed to load overpay_extremes_material.csv");
      }
      return response.text();
    })
  ])
    .then(function (payloads) {
      const walkRows = parseCsv(payloads[0]);
      const retentionRows = parseCsv(payloads[1]);
      const extremeRows = parseCsv(payloads[2]);

      renderWalkRows(walkRows, "all", "all");
      renderExtremes(extremeRows, "overpay");
      attachFilterHandlers(walkRows, retentionRows);

      if (overpayBtn) {
        overpayBtn.addEventListener("click", function () {
          renderExtremes(extremeRows, "overpay");
        });
      }

      if (discountBtn) {
        discountBtn.addEventListener("click", function () {
          renderExtremes(extremeRows, "discount");
        });
      }
    })
    .catch(function () {
      resultsNode.innerHTML = "";
      const message = document.createElement("p");
      message.className = "tmg-note";
      message.textContent = "Explorer data files are missing. Ensure deploy copies Phase 5 CSV outputs into dashboard/src/data.";
      resultsNode.appendChild(message);
    });
})();
