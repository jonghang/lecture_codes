
<!DOCTYPE HTML>
<html>

<head>
    <meta charset="utf-8">

    <title>forest_tools.py (editing)</title>
    <link rel="shortcut icon" type="image/x-icon" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/base/images/favicon.ico?v=97c6417ed01bdc0ae3ef32ae4894fd03">
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/jquery-ui/themes/smoothness/jquery-ui.min.css?v=9b2c8d3489227115310662a343fce11c" type="text/css" />
    <link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/jquery-typeahead/dist/jquery.typeahead.min.css?v=7afb461de36accb1aa133a1710f5bc56" type="text/css" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    
    
<link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/codemirror/lib/codemirror.css?v=f25e9a9159e54b423b5a8dc4b1ab5c6e">
<link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/codemirror/addon/dialog/dialog.css?v=c89dce10b44d2882a024e7befc2b63f5">

    <link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/style/style.min.css?v=29c09309dd70e7fe93378815e5f022ae" type="text/css"/>
    

    <link rel="stylesheet" href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/custom/custom.css" type="text/css" />
    <script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/es6-promise/promise.min.js?v=f004a16cb856e0ff11781d01ec5ca8fe" type="text/javascript" charset="utf-8"></script>
    <script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/preact/index.js?v=5b98fce8b86ce059de89f9e728e16957" type="text/javascript"></script>
    <script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/proptypes/index.js?v=c40890eb04df9811fcc4d47e53a29604" type="text/javascript"></script>
    <script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/preact-compat/index.js?v=d376eb109a00b9b2e8c0d30782eb6df7" type="text/javascript"></script>
    <script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/components/requirejs/require.js?v=6da8be361b9ee26c5e721e76c6d4afce" type="text/javascript" charset="utf-8"></script>
    <script>
      require.config({
          
          urlArgs: "v=20190829051133",
          
          baseUrl: '/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/',
          paths: {
            'auth/js/main': 'auth/js/main.min',
            custom : '/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/custom',
            nbextensions : '/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/nbextensions',
            kernelspecs : '/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/kernelspecs',
            underscore : 'components/underscore/underscore-min',
            backbone : 'components/backbone/backbone-min',
            jquery: 'components/jquery/jquery.min',
            bootstrap: 'components/bootstrap/js/bootstrap.min',
            bootstraptour: 'components/bootstrap-tour/build/js/bootstrap-tour.min',
            'jquery-ui': 'components/jquery-ui/ui/minified/jquery-ui.min',
            moment: 'components/moment/moment',
            codemirror: 'components/codemirror',
            termjs: 'components/xterm.js/dist/xterm',
            typeahead: 'components/jquery-typeahead/dist/jquery.typeahead.min',
          },
          map: { // for backward compatibility
              "*": {
                  "jqueryui": "jquery-ui",
              }
          },
          shim: {
            typeahead: {
              deps: ["jquery"],
              exports: "typeahead"
            },
            underscore: {
              exports: '_'
            },
            backbone: {
              deps: ["underscore", "jquery"],
              exports: "Backbone"
            },
            bootstrap: {
              deps: ["jquery"],
              exports: "bootstrap"
            },
            bootstraptour: {
              deps: ["bootstrap"],
              exports: "Tour"
            },
            "jquery-ui": {
              deps: ["jquery"],
              exports: "$"
            }
          },
          waitSeconds: 30,
      });

      require.config({
          map: {
              '*':{
                'contents': 'services/contents',
              }
          }
      });

      // error-catching custom.js shim.
      define("custom", function (require, exports, module) {
          try {
              var custom = require('custom/custom');
              console.debug('loaded custom.js');
              return custom;
          } catch (e) {
              console.error("error loading custom.js", e);
              return {};
          }
      })
    </script>

    
    

</head>

<body class="edit_app "
 
data-base-url="/hostip/172.31.24.197%3A6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/"
data-file-path="forest_tools.py"

  
 

dir="ltr">

<noscript>
    <div id='noscript'>
      Jupyter Notebook requires JavaScript.<br>
      Please enable it to proceed.
  </div>
</noscript>

<div id="header">
  <div id="header-container" class="container">
  <div id="ipython_notebook" class="nav navbar-brand pull-left"><a href="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/tree" title='dashboard'>
<img src='/hostip/172.31.24.197:6000/hub/logo' alt='Jupyter Notebook'/>
</a></div>

  

  
  

    <span id="login_widget">
      
        <button id="logout" class="btn btn-sm navbar-btn">Logout</button>
      
    </span>

  

  

<a href='/hostip/172.31.24.197:6000/hub/home'
 class='btn btn-default btn-sm navbar-btn pull-right'
 style='margin-right: 4px; margin-left: 2px;'
>
Control Panel</a>


  

<span id="save_widget" class="pull-left save_widget">
    <span class="filename"></span>
    <span class="last_modified"></span>
</span>


  </div>
  <div class="header-bar"></div>

  

<div id="menubar-container" class="container">
  <div id="menubar">
    <div id="menus" class="navbar navbar-default" role="navigation">
      <div class="container-fluid">
          <p  class="navbar-text indicator_area">
          <span id="current-mode" >current mode</span>
          </p>
        <button type="button" class="btn btn-default navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
          <i class="fa fa-bars"></i>
          <span class="navbar-text">Menu</span>
        </button>
        <ul class="nav navbar-nav navbar-right">
          <li id="notification_area"></li>
        </ul>
        <div class="navbar-collapse collapse">
          <ul class="nav navbar-nav">
            <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">File</a>
              <ul id="file-menu" class="dropdown-menu">
                <li id="new-file"><a href="#">New</a></li>
                <li id="save-file"><a href="#">Save</a></li>
                <li id="rename-file"><a href="#">Rename</a></li>
                <li id="download-file"><a href="#">Download</a></li>
              </ul>
            </li>
            <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Edit</a>
              <ul id="edit-menu" class="dropdown-menu">
                <li id="menu-find"><a href="#">Find</a></li>
                <li id="menu-replace"><a href="#">Find &amp; Replace</a></li>
                <li class="divider"></li>
                <li class="dropdown-header">Key Map</li>
                <li id="menu-keymap-default"><a href="#">Default<i class="fa"></i></a></li>
                <li id="menu-keymap-sublime"><a href="#">Sublime Text<i class="fa"></i></a></li>
                <li id="menu-keymap-vim"><a href="#">Vim<i class="fa"></i></a></li>
                <li id="menu-keymap-emacs"><a href="#">emacs<i class="fa"></i></a></li>
              </ul>
            </li>
            <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">View</a>
              <ul id="view-menu" class="dropdown-menu">
              <li id="toggle_header" title="Show/Hide the logo and notebook title (above menu bar)">
              <a href="#">Toggle Header</a></li>
              <li id="menu-line-numbers"><a href="#">Toggle Line Numbers</a></li>
              </ul>
            </li>
            <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Language</a>
              <ul id="mode-menu" class="dropdown-menu">
              </ul>
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</div>

<div class="lower-header-bar"></div>


</div>

<div id="site">


<div id="texteditor-backdrop">
<div id="texteditor-container" class="container"></div>
</div>


</div>






    


<script src="/hostip/172.31.24.197:6000/user/ccc_v1_w_OGJjO_86539_371430_371430_43259_0/static/edit/js/main.min.js?v=7eb6af843396244a81afb577aedbaf89" type="text/javascript" charset="utf-8"></script>


<script type='text/javascript'>
  function _remove_token_from_url() {
    if (window.location.search.length <= 1) {
      return;
    }
    var search_parameters = window.location.search.slice(1).split('&');
    for (var i = 0; i < search_parameters.length; i++) {
      if (search_parameters[i].split('=')[0] === 'token') {
        // remote token from search parameters
        search_parameters.splice(i, 1);
        var new_search = '';
        if (search_parameters.length) {
          new_search = '?' + search_parameters.join('&');
        }
        var new_url = window.location.origin + 
                      window.location.pathname + 
                      new_search + 
                      window.location.hash;
        window.history.replaceState({}, "", new_url);
        return;
      }
    }
  }
  _remove_token_from_url();
</script>

<script>
    require(['base/js/namespace'], function(Jupyter) {
        Jupyter._target = '_self';
    });

    require([
        'base/js/namespace',
        'base/js/events'
        ],
        function(Jupyter, events) {
            events.on("notebook_loaded.Notebook",
                function () {
                    Jupyter.notebook.set_autosave_interval(5000); //in milliseconds
                }
            );
            events.on("checkpoint_created.Notebook",
                function () {
                    console.log("VOC: checkpoint saved");
                    if (parent && typeof(parent.vocareum_be_notebook_checkpointed) === typeof(Function)) 
                    {
                        parent.vocareum_be_notebook_checkpointed(true);
                    }
                }
            );
            events.on("checkpoint_failed.Notebook",
                function () {
                    console.log("VOC: checkpoint failed");
                    if (parent && typeof(parent.vocareum_be_notebook_checkpointed) === typeof(Function)) 
                    {
                        parent.vocareum_be_notebook_checkpointed(false);
                    }
                }
            );
            // events.on("notebook_saving.Notebook",
            //     function () {
            //         console.log("VOC: notebook saving");
            //     }
            // );
            // events.on("before_save.Notebook",
            //     function () {
            //         console.log("VOC: before save");
            //     }
            // );
            // events.on("notebook_saved.Notebook",
            //     function () {
            //         console.log("VOC: notebook saved");
            //         if (parent && typeof(parent.vocareum_be_notebook_saved) === typeof(Function)) 
            //         {
            //             parent.vocareum_be_notebook_saved(true);
            //         }
            //     }
            // );
            // events.on("notebook_save_failed.Notebook",
            //     function () {
            //         console.log("VOC: notebook save failed");
            //         if (parent && typeof(parent.vocareum_be_notebook_saved) === typeof(Function)) 
            //         {
            //             parent.vocareum_be_notebook_saved(false);
            //         }
            //     }
            // );
        }
    );

</script>

<script>

console.log("****** CUSTOM VOCAREUM 6.0 *****");

var VOC = true;
var VOCDEBUG = false;
var vocext_comments = {};
var vocNotebookLoaded = false;

var vocDialog = null;
var vocModal = null;
var vocext_commentIcon = 'fa-plus-square-o';
var vocext_gutterClass = 'vocAddFlag';
var vocext_uniqUserName = 'unset';

if (VOC) {
        document.domain = "vocareum.com";
        parent.iframeWindow = window;
} 

var VOCRUBRICS = false; // rubrics and grading, not access to meta
if (parent && typeof(parent.vocareum_be_getRubricItems) === typeof(Function)) {
    VOCRUBRICS = true;
}

window.vocExtCheckNotebookAvailable = function() 
{
  console.log("VOC: Checking if notebook is open on this page");
  if (Jupyter && (typeof(Jupyter.notebook) != 'undefined') && (Jupyter.notebook != null))
  {
    return true;
  }
  return false;
}

window.vocExtSaveNotebook = function() 
{
  console.log("VOC: Saving notebook before submission");
  if (Jupyter && (typeof(Jupyter.notebook) != 'undefined') && (Jupyter.notebook != null))
  {
    Jupyter.notebook._checkpoint_after_save = false;
    Jupyter.notebook.save_notebook(true);
  }
  else
  {
    console.log("VOC: Nothing to save");
  }
}

window.vocExtSaveAndCheckpointNotebook = function() 
{
  console.log("VOC: Saving and CheckPointing notebook before submission");
  if (Jupyter && (typeof(Jupyter.notebook) != 'undefined') && (Jupyter.notebook != null))
  {
    Jupyter.notebook._checkpoint_after_save = true;
    Jupyter.notebook.save_notebook(true);
  }
  else
  {
    console.log("VOC: nothing to save/checkpoint");
  }
  // var el = document.getElementById("save_checkpoint");
  // if ((typeof(el) != 'undefined') && (el != null)) 
  // {
  //   $('#save_checkpoint').click();
  // } 
}

window.vocExtGetUniqUserName = function() {
  return vocext_uniqUserName;
}

if (parent)
{
  hrf = window.location.href;
  if (hrf)
  {
    // this is of the form: http[s]://<something>.vocareum.com[:port]/[hostip/1.2.3.4:port/]user/<username>[/...]
    var re = /^http[^\/]*\/\/[^\/]+\/(?:hostip\/[^\/]+\/)?user\/([^\/]+)/;
    var result = re.exec(hrf);
    if (result && (result.length >= 1))
    {
      username = result[1];
      console.log("VOC: username: " + username);
      vocext_uniqUserName = username;
    }
  }
}

function vocext_updateStorage2(cell, cellid, index, obj, isAdd) {
    return;
    if (isAdd) {
        var newobj = {};
        newobj.author = obj.author;
        newobj.comment = obj.comment;
        newobj.line = obj.line;
        newobj.timestamp = obj.timestamp;
        
        if (!(cellid in vocext_comments.code_comments)) {
            vocext_comments.code_comments[cellid] = {};
        }
        vocext_comments.code_comments[cellid][index] = newobj;
    } else {
        delete vocext_comments.code_comments[cellid][index];
    }    
}

// inform vocareum
function vocext_updateStorage(cell, cellid, index, obj, isAdd) {
    if (!VOC) return;
    if (isAdd) {
        window.parent.vocareum_be_addComment(Jupyter.notebook.notebook_path, cellid, index);
    } else {
        window.parent.vocareum_be_deleteComment(obj.commentid, cellid, index);
    }
}
        
function vocext_deleteComment(isClient, cellid, index) {
    if (!VOC) return;

    var obj = vocext_comments.session_data[cellid][index];
    if (!isClient) {
        console.log("Clearing local comment");
        obj.lineWidget.clear();
        $('#voccomment-' + cellid + '-' + index).remove();
        delete vocext_comments.session_data[cellid][index];
    } else {
        console.log("Clearing server comment");
        vocext_updateStorage(null, cellid, index, obj, false);
    }
}


function vocext_editDialog(cellid, index, line, comment, commentid, author) {
      var commentStr = comment;
      var entry_box = $('<textarea rows="10" cols="60">'+commentStr+'</textarea>');
      var entry_linenum = $('<input type="hidden" name="linenum" value="'+line+'" />');
      var entry_cellid = $('<input type="hidden" name="cellid" value="'+cellid+'" />');

    var dialog_body = $("<div/>").append("")
        .append($("<form/>").append(entry_linenum).append(entry_cellid).append(entry_box));

    vocDialog.modal({
        notebook: Jupyter.notebook,
        keyboard_manager: Jupyter.keyboard_manager,
        title : "Edit code comment",
        body : dialog_body,
        open: function() {
            entry_box.focus();
        },
        buttons : {
            "Save Comment" : {
                class : "btn-primary pull-left",
                click : function() { 
                    if (VOCDEBUG) console.log($(entry_box).val()); 
                    vocext_updateComment(cellid, index, parseInt($(entry_linenum).val()), $(entry_box).val(), author, null, commentid, vocext_comments.context['myid']);
                },
            }
        }
    });   
    
}

function vocext_editComment(cellid, index) {
    if (!VOC) return;

    var obj = vocext_comments.session_data[cellid][index];
    vocext_editDialog(cellid, index, obj.line, obj.comment, obj.commentid, obj.author);

}


function vocext_updateComment(cellid, index, linenum, comment, author, when, commentid, authorid) {
    var obj = vocext_comments.session_data[cellid][index];

    var tmpDom  = $(obj.lineWidget.node);
        
    vocext_comments.session_data[cellid][index].author = author;
    vocext_comments.session_data[cellid][index].authorid = vocext_comments.context['myid'];   
    vocext_comments.session_data[cellid][index].comment = comment;   

    vocext_updateStorage(null, cellid, index, vocext_comments.session_data[cellid][index], true);
        
    var authorLine = '<strong style="color: #3b7fd4;margin-left:5px" class="vocCommentLine_authorline">'+author+', just now</strong>';
    var commentLine = comment;
    tmpDom.find('.vocCommentLine_authorline').html(authorLine);
    tmpDom.find('.vocCommentLine_commentline').html(commentLine);
                
    var markdown_el = $('#voccomment-' + cellid + '-' + index);
    if (markdown_el) {
        markdown_el.find('.vocCommentLine_authorline').html(authorLine);
        markdown_el.find('.vocCommentLine_commentline').html(commentLine);
    }               
}

function vocext_addComment(cell, linenum, comment, author, when, commentid, authorid) {
            var index = vocext_store_codecomment(cell, linenum, comment, author, when, authorid);
            var commentObj = vocext_comments.session_data[cell.cell_id][index] ;     
            var nodeStr = vocext_getCodeCommentBox(cell, false, commentObj);
            var node = $(nodeStr)[0];

            if (VOCDEBUG) console.log(cell);
            if (VOCDEBUG) console.log(linenum);
            if (VOCDEBUG) console.log(comment);
        
            var linewidget = cell.code_mirror.addLineWidget(linenum, node, {above: false, coverGutter: false, noHScroll: true});
            
            if (cell.cell_type == 'markdown') {
                var markDownStr =  vocext_getCodeCommentBox(cell, true, commentObj);            
                cell.element.after(markDownStr);
            }
            
            vocext_comments.session_data[cell.cell_id][index].lineWidget = linewidget;   
            vocext_comments.session_data[cell.cell_id][index].commentid = commentid;   
            
            if (commentid == 0) {
                vocext_updateStorage(cell, cell.cell_id, index, commentObj, true);
            }            

}

function vocext_got_confirmation(action, passed, cellid, index, lastid) {
    if (action == 'add') {
        if (passed) {
           vocext_comments.session_data[cellid][index].commentid = lastid;
           vocModal.modal('hide');
        } else {
            vocext_deleteComment(false, cellid, index);
            vocModal.find('.vocClassAddComment').html('Failed ! Save again');
            vocModal.find('.vocClassAddComment').addClass('btn-danger');
        }
    }
    if (action == 'delete') {
        if (passed) {
           vocext_deleteComment(false, cellid, index);
        } else {
        }
    }

}

function vocext_get_codecomment_object(index, line, comment, author, timestamp, authorid) {
    var obj = {};
    obj.index = index;
    obj.line = line;
    obj.author = author;
    obj.authorid = authorid;
    obj.comment = comment;
    obj.timestamp = timestamp;
    return obj;
}

function vocext_store_codecomment(cell, line, comment, author, when, authorid) {
    var d = new Date();
    var ts = d.getTime();
    var cellid = cell.cell_id;
    var index = ts + "_" + d.getMilliseconds();
    var obj = vocext_get_codecomment_object(index, line, comment, author, when, authorid);

    // vocext_comments.session_data[cellid].push(obj);
    vocext_comments.session_data[cellid][index] = obj;
    
    return index;
}

function vocext_getCodeCommentBox(cell, flag, commentObj) {
    var markdown_class = '';
    var markdown_id_code = '';
    var markdown_style_bg = '';
    var markdown_style_display = '';
    if (flag) {
        markdown_class = "voccomment-markdown-" + cell.cell_id;
        markdown_id_code = ' id="voccomment-' + cell.cell_id + '-' + commentObj.index + '" ';

        if (!(cell.rendered)) {
            markdown_style_display = 'display:none;';
        }    
            
        markdown_style_bg = 'background-color:rgb(245, 245, 245);';
    }   
    
    var when =  ((commentObj.timestamp == null) ? "just now" : (commentObj.timestamp + " ago"));
    
     var timeStr = when; // moment(commentObj.timestamp).fromNow();
     var delBtn = '';

     if ( (vocext_comments.context['myid'] != commentObj.authorid) && !vocext_comments.context['codecomment_edit'] ) {
     } else if (parent && typeof(parent.vocareum_be_deleteComment) === typeof(Function)) {
         delBtn = '<button onclick="vocext_deleteComment(true, \''+ cell.cell_id +'\',\''+ commentObj.index + '\');" data-cellid="'+ cell.cell_id + '" data-index="' + commentObj.index  +'" class="vocCommentLineDelete pull-right" style="border: medium none; background: transparent none repeat scroll 0% 0%;" ><i class="fa fa-times"></i></button>';
         delBtn += '<button onclick="vocext_editComment(\''+ cell.cell_id +'\',\''+ commentObj.index + '\');" data-cellid="'+ cell.cell_id + '" data-index="' + commentObj.index  +'" class="vocCommentLineEdit pull-right" style="border: medium none; background: transparent none repeat scroll 0% 0%;" ><i class="fa fa-edit"></i></button>';

     }
     var nodeStr = '\
        <div class="vocCommentLine '+markdown_class+'" ' + markdown_id_code + 'style=" ' + markdown_style_display + markdown_style_bg + '">\
        <strong style="color: #3b7fd4;margin-left:5px" class="vocCommentLine_authorline">'+ commentObj.author + ', ' + timeStr  +'</strong> ' + delBtn + '   \
        <pre style="padding-top: 4px; border:none; background-color:none; margin-left:5px" class="vocCommentLine_commentline">' + commentObj.comment + '</pre> </div>'; 
     return nodeStr;     
}

function vocext_dashBoardUpdate() {
        if (($('#login_widget').next().attr('href')) == '/hub/home') {
            $('#login_widget').next().hide();
        }
        $('#login_widget').hide();
        $('#new-terminal').hide();
        if (!(parent.vocareum_be_canEdit())) {
             $('#notebook_toolbar').hide();
        }
}

// GRADING            
var vocExtRubricItems = {};
var vocExtRubricScores = {};
if (VOCRUBRICS && parent && typeof(parent.vocareum_be_getRubricItems) === typeof(Function)) {
    vocExtRubricItems = parent.vocareum_be_getRubricItems(); // GRADING
    console.log(vocExtRubricItems);
    vocExtRubricScores = parent.vocareum_be_getRubricScores();
    console.log(vocExtRubricScores);
}

var vocgradeRubricNameIdPrefix = 'vocgrade-rubric-name-';
var vocgradeRubricScoreIdPrefix = 'vocgrade-rubric-score-';

require([
    'base/js/namespace',
    'base/js/events',    
    'base/js/utils',
    'base/js/dialog',
    'moment'
], function(Jupyter, events, utils, dialog, moment) {

    // DO NOT COMMIT
    // Jupyter._target = '_self'; 

    vocDialog = dialog;
    
    events.on('app_initialized.DashboardApp', function(){
        vocext_dashBoardUpdate();
    });

    events.on('app_initialized.NotebookApp', function(){
        if (VOCDEBUG) console.log("VOC app_initialized.NotebookApp");
        vocext_init(events);
        vocext_dashBoardUpdate();
    });


var vocCellIndex = 0;

function vocext_config_codemirror() {
    if (VOCDEBUG) console.log("configuring codemirror");
    
    var cells = Jupyter.notebook.get_cells();

    var grade_opt_enable_str = ' disabled ';
    var can_edit_rubrics = false;
    
    if ( (typeof(parent.vocareum_be_canEditRubrics) === typeof(Function)) && parent.vocareum_be_canEditRubrics() ) { // only if present
      grade_opt_enable_str = ' ';
      can_edit_rubrics = true;
    }      

    cells.forEach( function (cell) {
      vocext_config_codemirror_one(cell, grade_opt_enable_str, can_edit_rubrics);
    });
}

function vocext_config_codemirror_for_cell(cell) {
    if (VOCDEBUG) console.log("configuring codemirror for cell");

    var grade_opt_enable_str = ' disabled ';
    var can_edit_rubrics = false;
    
    if ( (typeof(parent.vocareum_be_canEditRubrics) === typeof(Function)) && parent.vocareum_be_canEditRubrics() ) { // only if present
      grade_opt_enable_str = ' ';
      can_edit_rubrics = true;
    }      
    
    vocext_config_codemirror_one(cell, grade_opt_enable_str, can_edit_rubrics);
}

function vocIsSolutionCell(cell) {
    if (cell.metadata.nbgrader.solution === undefined) {
        return false;
    } else {
        return cell.metadata.nbgrader.solution;
    }
}

function vocIsGradeCell(cell) {
    if (cell.metadata.nbgrader.grade === undefined) {
        return false;
    } else {
        return cell.metadata.nbgrader.grade;
    }
}

function vocIsLockedCell(cell) {
    if (vocIsSolutionCell(cell)) {
        return false;
    } else if (vocIsGradeCell(cell)) {
        return true;
    } else if (cell.metadata.nbgrader.locked === undefined) {
        return false;
    } else {
        return cell.metadata.nbgrader.locked;
    }
}


/* callbacks */
function vocext_config_codemirror_one(cell, grade_opt_enable_str, can_edit_rubrics) {

          vocCellIndex++;

          var cellid = cell.cell_id;
          var config = cell.config;
          var events = cell.events;
          
          if (!(cellid in vocext_comments.session_data)) {
              vocext_comments.session_data[cellid] = {};
          }
          vocext_comments.session_data[cellid]["cell_number"] = vocCellIndex;

          if (parent) parent.vocareum_be_loadComments(Jupyter.notebook.notebook_path, cell, vocCellIndex);

          if (cell.cell_type == 'markdown') {
             if (cell.rendered) {
                 if (VOCDEBUG) console.log("MARKDOWN rendered at init");
             }
          }

          var metaVal = null;
          if (typeof cell.metadata.nbgrader != 'undefined') {
              if (vocIsSolutionCell(cell) && vocIsGradeCell(cell)) {
                  metaVal = "manual";
              } else if (vocIsSolutionCell(cell) && cell.cell_type === "code") {
                  metaVal = "xsolution";
              } else if (vocIsGradeCell(cell) && cell.cell_type === "code") {
                  metaVal = "tests";
              } else if (vocIsLockedCell(cell)) {
                  metaVal = "readonly";
              } else {
                  metaVal = "";
              }                  
          }
              
          if (!can_edit_rubrics) {
              if ((metaVal === 'readonly') || (metaVal === 'tests')) {
                  cell.code_mirror.setOption("readOnly", "nocursor");   
              }
          }          
          
          if (VOCRUBRICS) {
              var metaVal2 = null;
              if ( (typeof cell.metadata.nbgrader != 'undefined') && (typeof cell.metadata.nbgrader.grade_id != 'undefined') ) {
                  metaVal2 = cell.metadata.nbgrader.grade_id;
              }     

              var metaPoints2 = null;
              if ( (typeof cell.metadata.nbgrader != 'undefined') && (typeof cell.metadata.nbgrader.points != 'undefined') ) {
                  metaPoints2 = cell.metadata.nbgrader.points;
              }     

              // var vocScore = ''; // if any rubric item has been selected, this variable should hold it's value

              var vocext_gradeInnerText = '';
              
              var vocext_gradeRubricDisplay = "";
              if (metaVal != 'tests') {
                  vocext_gradeRubricDisplay = " display: none ";
              }

              // var vocext_gradeRubricDropDown = '<div class="button_container"><span><select class="vocgrade-select-rubrics" style="margin-right:3px;' + vocext_gradeRubricDisplay + '"><option value="">-</option>';
              // 
              // var foundIt = false;
              // for  (var key in vocExtRubricItems) {
              //     var sel = ' '; 
              //     score = vocExtRubricScores[key];
              //     name = vocExtRubricItems[key];
              //     if (metaVal2 && (vocExtRubricItems[key].toLowerCase() == metaVal2.toLowerCase())) {
              //         sel = ' selected ';
              //         foundIt = true;
              //         name = metaVal2;
              //         if (metaPoints2) {
              //           score = metaPoints2;
              //           vocScore = metaPoints2;
              //         }
              //     }
              //     vocext_gradeRubricDropDown = vocext_gradeRubricDropDown + '<option value="' + key + '"' + sel + grade_opt_enable_str + '>' + name + ' (' + score + ')' + '</option>';
              // }
              // 
              // if (!foundIt && metaVal2) {
              //   sel = ' selected ';
              //   grade_id = metaVal2;
              //   score = 0;
              //   if (metaPoints2) {
              //     score = metaPoints2;
              //     vocScore = metaPoints2;
              //   }
              //   vocext_gradeRubricDropDown = vocext_gradeRubricDropDown + '<option value="' + grade_id + '"' + sel + grade_opt_enable_str + '>' + grade_id + ' (' + score + ')' + '</option>';
              // }
              //     
              // vocext_gradeRubricDropDown = vocext_gradeRubricDropDown + '</select></span></div>';


              existingRubricName = metaVal2 ? metaVal2 : '';

              vocgradeRubricNameId = vocgradeRubricNameIdPrefix + cellid;
              var vocext_gradeRubricNameElement = '<div class="button_container"><span class="tags-input"><input type="text" placeholder="Name" style="min-width:80px; border-radius:5px; text-align:left;margin-right:3px;' + vocext_gradeRubricDisplay + '" class="vocgrade-rubric-name" id="' + vocgradeRubricNameId + '" ' + grade_opt_enable_str + ' ' + '" value="' + existingRubricName + '" >';
              vocext_gradeRubricNameElement = vocext_gradeRubricNameElement + '</span></div>';

              existingRubricScore = metaPoints2 ? metaPoints2 : '';

              vocgradeRubricScoreId = vocgradeRubricScoreIdPrefix + cellid;
              var vocext_gradeRubricScoreElement = '<div class="button_container"><span class="tags-input"><input type="text" placeholder="Points" style="width:50px; border-radius:5px; text-align:right;margin-right:3px;' + vocext_gradeRubricDisplay + '" class="vocgrade-rubric-score" id="' + vocgradeRubricScoreId + '" ' + grade_opt_enable_str + ' ' + '" value="' + existingRubricScore + '" >';
              vocext_gradeRubricScoreElement = vocext_gradeRubricScoreElement + '</span></div>';


              // var opt_manual = '<option value="manual" ' + (metaVal === 'manual' ? ' selected ': '') + grade_opt_enable_str + '>Manually graded answer</option>';
              var opt_readonly = '<option value="readonly" ' + (metaVal === 'readonly' ? ' selected ': '') + grade_opt_enable_str + '>Read-only</option>';
              // var opt_solution = '<option value="xsolution"' + (metaVal === 'xsolution' ? ' selected ': '') + grade_opt_enable_str + '>Autograded answer</option>';
              var opt_tests = '<option value="tests" '+ (metaVal === 'tests' ? ' selected ': '') + grade_opt_enable_str + '>Autograder tests</option> ';

              var opt_solution = '<option value="" ' + (((metaVal !== 'tests') && (metaVal !== 'readonly')) ? ' selected ' : '') + grade_opt_enable_str + '>Solution</option> ';
              
              if (cell.cell_type == 'markdown') {
                  vocext_gradeInnerText = '<div class="vocgrade celltoolbar" style="background-color: lightblue;"><div class="button_container"><span><select class="vocgrade-select">' + opt_solution + opt_readonly + '</select></span></div></div>';
                  // vocext_gradeInnerText = '<div class="vocgrade celltoolbar" style="background-color: lightblue;"><div class="button_container"><span><select class="vocgrade-select"><option value="">-</option>'+ opt_manual + opt_readonly + '</select></span></div></div>';
              } else {
                  vocext_gradeInnerText = '<div class="vocgrade celltoolbar" style="background-color: lightblue;">'+ vocext_gradeRubricScoreElement + vocext_gradeRubricNameElement +'<div class="button_container"><span><select class="vocgrade-select">' + opt_solution + opt_tests + opt_readonly + '</select></span></div></div>';
                  // vocext_gradeInnerText = '<div class="vocgrade celltoolbar" style="background-color: lightblue;">'+ vocext_gradeRubricDropDown +'<div class="button_container"><span><select class="vocgrade-select"><option value="">-</option>'+ opt_manual + opt_solution + opt_tests + opt_readonly + '</select></span></div></div>';
              }
              cell.element.find('.inner_cell').prepend(vocext_gradeInnerText);   
              Jupyter.keyboard_manager.register_events($('#' + vocgradeRubricNameId));
              Jupyter.keyboard_manager.register_events($('#' + vocgradeRubricScoreId));
          }
// GRADE END          

          if (parent && typeof(parent.vocareum_be_addComment) === typeof(Function)) {
              cell.code_mirror.on('cursorActivity', vocext_handleEditorCursorActivity.bind(this, cell));
              cell.code_mirror.on('gutterClick', vocext_handleEditorGutterClick.bind(this, cell));
          }

          if (!(parent.vocareum_be_canEdit())) { // document
              cell.code_mirror.setOption("readOnly", "nocursor");
          }

}       

                    
function vocext_init(events) {
    if (VOCDEBUG) console.log("vocext_init");
    if (VOC) {
        // document.domain = "vocareum.com";
        // parent.iframeWindow = window;
    
        parent.vocareum_be_loadContext();
    } else {
        parent = null;
        vocext_comments.context = {};
    }       
    events.on('command_mode.Cell', function(event, data) {
        if (VOCDEBUG) console.log("COM MODE");
        if (VOCDEBUG) console.log("Is cell rendered: " + data.cell.rendered);
    });
    events.on('edit_mode.Cell', function(event, data) {
        if (VOCDEBUG) console.log("EDIT MODE");
        if (VOCDEBUG) console.log("Is cell rendered: " + data.cell.rendered);
        $('.voccomment-markdown-'+data.cell.cell_id).hide();

    });
    
    events.on('rendered.MarkdownCell', function(event, data) {
        if (VOCDEBUG) console.log("RENDERED");
        $('.voccomment-markdown-'+data.cell.cell_id).show();
    });   
    
    events.on('notebook_loaded.Notebook', function(event, data) {
        if (VOCDEBUG) console.log("LOADED");
        vocNotebookLoaded = true;
        vocext_config_codemirror();
    });   
     
    events.on('notebook_saved.Notebook', function(event, data) {
        if (VOCDEBUG) console.log("SAVED");
    });            
 
    events.on('create.Cell', function(event, data) {
        if (VOCDEBUG) console.log("CREATE CELL");
        if (vocNotebookLoaded) {
          if (VOCDEBUG) console.log("After Loading complete");
          vocext_config_codemirror_for_cell(data.cell);
        }
    });            
              
    vocext_comments.session_data =  {};

    if (!(parent.vocareum_be_canEdit())) {
        $('#menubar-container').hide();
    }
}   

function vocext_handleEditorCursorActivity(cell, cm) {
    // if (cell.mode == 'command') return; // TODO: change command to defined
    if (VOCDEBUG) console.log("vocext_handleEditorCursorActivity");
                
    if (cell.code_mirror.getOption("gutters").indexOf(vocext_gutterClass) == -1) {
        cell.code_mirror.setOption("gutters", [vocext_gutterClass]);  
    } 
    
    var pos = cm.doc.getCursor();
    var thisLineNum = pos.line;
    
    cm.clearGutter('vocAddFlag');
    cm.setGutterMarker(pos.line, 'vocAddFlag', $('<b class="vocAddFlag fa '+vocext_commentIcon+'"></b>')[0]);
    
}

function vocext_handleEditorGutterClick(cell, cm, line, gutter, e) {
    if (VOCDEBUG) console.log("vocext_handleEditorGutterClick");
    if (!($(e.target).hasClass(vocext_commentIcon))) {
        if (VOCDEBUG) console.log("vocext_handleEditorGutterClick yes ");
    } 

    var B = document.body,
        H = document.documentElement,
        scrollHeight,
        yMousePos
    
    if (typeof document.height !== 'undefined') {
        scrollHeight = document.height // For webkit browsers
    } else {
        scrollHeight = Math.max(B.scrollHeight, B.offsetHeight,H.clientHeight, H.scrollHeight, H.offsetHeight);
    }
    yMousePos = e.clientY;
  
    var entry_box = $('<textarea rows="10" cols="60"/>');
    var entry_linenum = $('<input type="hidden" name="linenum" value="'+line+'" />');
    var entry_cellid = $('<input type="hidden" name="cellid" value="'+cell.cell_id+'" />');

    var dialog_body = $("<div/>").append("")
        .append($("<form/>").append(entry_linenum).append(entry_cellid).append(entry_box));

    vocModal = dialog.modal({
        notebook: Jupyter.notebook,
        keyboard_manager: Jupyter.keyboard_manager,
        title : "Add a code comment",
        body : dialog_body,
        open: function() {
            entry_box.focus();
        },
        buttons : {
            "Save Comment" : {
                class : "btn-primary pull-left vocClassAddComment",
                click : function() { 
                    if (VOCDEBUG) console.log($(entry_box).val()); 
                    vocext_addComment(cell, parseInt($(entry_linenum).val()), $(entry_box).val(), vocext_comments.context['myname'], null, 0, vocext_comments.context['myid']);
                    return false;
                },
            }
        }
    });                        

    var loc = yMousePos;
    if (scrollHeight - loc < 300) {
        loc = loc - 300;
    }
    console.log("Dialog: height:" + scrollHeight + " / y:" + yMousePos + " / loc:" + loc);
    vocModal.css('top', loc);
}        
        


/* end callbacks */  

});

// GRADE
function vocext_gradeSetMeta(el, isType) {
    var el_cell = el.parents(".cell");

    if (el_cell.data().cell.metadata.nbgrader === undefined) {
        el_cell.data().cell.metadata.nbgrader = {};
    }
    if (isType) {
        var val = el.val();
        // el_cell.data().cell.metadata.nbgrader.vocGrading = val;
        
        if (val === "") {
            el_cell.data().cell.metadata.nbgrader.solution = false;
            el_cell.data().cell.metadata.nbgrader.grade = false;
            el_cell.data().cell.metadata.nbgrader.locked = false;
        } else if (val === "manual") {
            el_cell.data().cell.metadata.nbgrader.solution = true;
            el_cell.data().cell.metadata.nbgrader.grade = true;
            el_cell.data().cell.metadata.nbgrader.locked = false;
        } else if (val === "xsolution") {
            el_cell.data().cell.metadata.nbgrader.solution = true;
            el_cell.data().cell.metadata.nbgrader.grade = false;
            el_cell.data().cell.metadata.nbgrader.locked = false;
        } else if (val === "tests") {
            el_cell.data().cell.metadata.nbgrader.solution = false;
            el_cell.data().cell.metadata.nbgrader.grade = true;
            el_cell.data().cell.metadata.nbgrader.locked = true;
        } else if (val === "readonly") {
            el_cell.data().cell.metadata.nbgrader.solution = false;
            el_cell.data().cell.metadata.nbgrader.grade = false;
            el_cell.data().cell.metadata.nbgrader.locked = true;
        }    
    } else {
        el_cell.data().cell.metadata.nbgrader.grade_id = vocExtRubricItems[el.val()];
        el_cell.data().cell.metadata.nbgrader.points = vocExtRubricScores[el.val()];
        // need to show these points in the input box
        txtBox = '#' + vocgradeRubricScoreIdPrefix + el_cell.data().cell.cell_id;
        $(txtBox).val(vocExtRubricScores[el.val()]);
        console.log("Txtbox = " + txtBox + " / val = " + vocExtRubricScores[el.val()]);
    }
    console.log("Setting meta data");
    console.log(el_cell.data().cell.metadata);
    Jupyter.notebook.set_dirty(true);
}

  $('body').on('change', '.vocgrade-select', function() {
    var $this = $(this);
    console.log($this);
    console.log($this.parents(".cell").data().cell);
    
    var el = $this.closest('.vocgrade');
    if ( ($this.val() == 'xsolution') || ($this.val() == 'tests') ) {
      // el.find('.vocgrade-select-rubrics').show();
      el.find('.vocgrade-rubric-score').show();
      el.find('.vocgrade-rubric-name').show();
    } else {
      // el.find('.vocgrade-select-rubrics').hide();
      el.find('.vocgrade-rubric-score').hide();
      el.find('.vocgrade-rubric-name').hide();
    }
    vocext_gradeSetMeta($this, true);
    return false;
  })
  
  // $('body').on('change', '.vocgrade-select-rubrics', function() {
  //   var $this = $(this);
  //   console.log($this);
  //   console.log($this.parents(".cell").data().cell);
  //   
  //   vocext_gradeSetMeta($this, false);
  //   return false;
  // })		

  var vocgradeRubricScoreTimeout = null;

  function vocgradeRubricScoreSetter(cell, val)
  {
    console.log("Set Score: " + val);
    cell.metadata.nbgrader.points = val;
    Jupyter.notebook.set_dirty(true);
  }

  $('body').on('keyup', '.vocgrade-rubric-score', function() {
    var $this = $(this);
    
    clearTimeout(vocgradeRubricScoreTimeout);

    cell = $this.parents(".cell").data().cell;
    if (cell.metadata.nbgrader !== undefined) {
      vocgradeRubricScoreTimeout = setTimeout(vocgradeRubricScoreSetter, 500, cell, $this.val());
    }

    return false;
  })		

  var vocgradeRubricNameTimeout = null;

  function vocgradeRubricNameSetter(cell, val)
  {
    console.log("Set Name: " + val);
    cell.metadata.nbgrader.grade_id = val;
    Jupyter.notebook.set_dirty(true);
  }

  $('body').on('keyup', '.vocgrade-rubric-name', function() {
    var $this = $(this);
    // console.log($this);
    // console.log($this.parents(".cell").data().cell);
    
    clearTimeout(vocgradeRubricNameTimeout);

    cell = $this.parents(".cell").data().cell;
    if (cell.metadata.nbgrader !== undefined) {
      vocgradeRubricNameTimeout = setTimeout(vocgradeRubricNameSetter, 500, cell, $this.val());
    }

    return false;
  })		

// GRADE END

/*
*
*/
</script>

</body>

</html>