(function(welcomeContent, lecture1Content, lecture2Content, lecture3Content, lecture4Content, loadHandler, evalHandler) {
  setTimeout(function(){
    //setup editor
    require(['vs/editor/editor.main'], function() {
      monaco.languages.register({ id: 'cubicaltt' });
      monaco.languages.setMonarchTokensProvider('cubicaltt', {
        keywords: [
          'hdata', 'data', 'import', 'mutual', 'let', 'in'
          , 'split', 'with', 'module', 'where', 'U', 'opaque'
          , 'transparent', 'transparent_all', 'token'
        ],
        operations:
          [ 'PathP', 'comp', 'transport', 'fill', 'Glue'
          , 'glue', 'unglue', 'Id', 'idC', 'id' ],
        operators:
          [ ':', '->', '=', '|', '\/', '/\\', '\\', '*', '_', '<', '>', '@', '-' ],
        symbols: /[=><!~?:&|+\-*\/\^%]+/,
        tokenizer: {
          root: [
            [ /[A-Za-z_$][\w$]*/, { cases:
              { '@keywords': 'keyword',
                '@operations': 'operation',
                '@default': 'identifier' }
              }
            ],
      
            ['undefined', 'special'],

            [/\\\//, 'operator'],
            [/\\/, 'operator'],
            [/\/\\/, 'operator'],
      
            { include: '@whitespace' },
      
            [/@symbols/, { cases: { '@operators': 'operator',
                                    '@default'  : '' } } ],
          ],
          comment: [
            [/[^-}]+/, 'comment' ],
            ['-}',    'comment', '@pop'  ],
            [/[-}]/,   'comment' ]
          ],
          whitespace: [
            [/[\\t\\r\\n]+/, 'white'],
            [/\{-/, 'comment', '@comment' ],
            [/--.*$/, 'comment'],
          ],
        }
      });
    // create theme
    monaco.editor.defineTheme('cubicaltt', {
      base: 'vs',
      inherit: false,
      rules: [
        { token: 'keyword', foreground: '952795' },
        { token: 'operation', foreground: '483d8b' },
        { token: 'operator', foreground: 'a0522d' },
        { token: 'special', foreground: 'ff2500' },
        { token: 'comment', foreground: 'b22222' }
    ]});
    // language config
    monaco.languages.setLanguageConfiguration('cubicaltt', {
      comments: {
        lineComment: '//',
        blockComment: ['{-', '-}'] 
    }
    });

    // create editor
    var editor = monaco.editor.create(document.getElementById('editor'), {
      model: null,
      value: "",
      language: 'cubicaltt',
      theme: 'cubicaltt',
    //   wordWrap: 'wordWrapColumn',
    //   wordWrapColumn: 75,
    //   wordWrapMinified: true,
    //   wrappingIndent: 'same',
      minimap: { enabled: false }
    });
    // add editor action to load buffer
    editor.addAction({
      id: 'load-buffer',
      label: 'load buffer',
      keybindings: [ monaco.KeyMod.WinCtrl | monaco.KeyCode.KEY_L ],
      run: function(ed) {
        loadHandler(app.current_tab.slice(0, -4), ed.getModel().getValue());
      }
    });
    editor.onDidChangeModelContent(function (e) {
      localStorage.removeItem("cache-module-" + app.current_tab.slice(0, -4));
    });

    var welcome = monaco.editor.createModel(welcomeContent, 'cubicaltt');
    editor.setModel(welcome);

    var lecture1 = monaco.editor.createModel(lecture1Content, 'cubicaltt');
    var lecture2 = monaco.editor.createModel(lecture2Content, 'cubicaltt');
    var lecture3 = monaco.editor.createModel(lecture3Content, 'cubicaltt');
    var lecture4 = monaco.editor.createModel(lecture4Content, 'cubicaltt');

    if (app === undefined) {
      app = {}
    }

    app.log_buffer = [];
    app.editor = editor;
    app.history_pre = [];
    app.history_post = [];
    app.current_tab = "welcome-tab";
    app.tabs = {
      "welcome-tab": [welcome, undefined],
      "lecture1-tab": [lecture1, undefined],
      "lecture2-tab": [lecture2, undefined],
      "lecture3-tab": [lecture3, undefined],
      "lecture4-tab": [lecture4, undefined]
    };

    var input = document.getElementById("eval-input");
    var button = document.getElementById("eval-button");
    button.addEventListener("click", function(event) {
      evalHandler(app.current_tab.slice(0, -4), input.value);
      app.history_pre.push(input.value);
      input.value = "";
    });
    input.addEventListener("keyup", function(event) {
      if (event.keyCode === 38) {
        // arrow up
        event.preventDefault();
        if (app.history_pre.length > 0) {
          var val = app.history_pre.pop()
          app.history_post.push(val);
          input.value = val;  
        }
      } else if (event.keyCode === 40) {
        // arrow down
        event.preventDefault();
        if (app.history_post.length > 0) {
          var val = app.history_post.pop()
          app.history_pre.push(val);
          input.value = val;
        }
      } else if (event.keyCode === 13) {
        event.preventDefault();
        button.click();
      }
    });

    function switchTab(tabName) {
      var navLinks = document.querySelectorAll(".tab-link");
      navLinks.forEach(navLink => {
        navLink.classList.remove("active");
      });
      var link = document.getElementById(tabName);
      link.classList.add('active');
      var state = editor.saveViewState();
      app.tabs[app.current_tab][1] = state;
      editor.setModel(app.tabs[tabName][0]);
      app.current_tab = tabName;
      editor.restoreViewState(app.tabs[tabName][1]);
    };

    var navLinks = document.querySelectorAll(".tab-link");
    navLinks.forEach(navLink => {
      navLink.addEventListener("click", function(event) {
        switchTab(navLink.id);
      });
    });

    function flushBuffer() {
      setTimeout(function() {
        var msg = [];
        app.log_buffer.forEach(item => {
          msg.push(item);
        });
        app.log_buffer = [];
        if (msg.length != 0) {
          document.getElementById('results').textContent += msg.join('');
          // scroll log
          var rc = document.getElementById('results-container');
          rc.scrollTop = rc.scrollHeight;
        }
        flushBuffer();
      }, 100);
    };
    flushBuffer();
    // end set timeout
    })
  }, 1000);})