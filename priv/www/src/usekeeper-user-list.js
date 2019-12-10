/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */


import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@polymer/paper-fab/paper-fab.js';
import './usekeeper-icons.js';
import './style-element.js';

class userList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="userGrid"
					loading="{{loading}}">
				<vaadin-grid-column>
					<template class="header">
						First 
					</template>
					<template>[[item.givenName]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Last 
					</template>
					<template>[[item.lastName]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Username
					</template>
					<template>[[item.id]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="my-icons:add"
					on-tap = "_userAddOpenModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getUserAjax"
				url="party/v4/individual"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			},
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('userGrid');
		grid.dataProvider = this._getLog;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.userGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.userGrid.selectedItems = [];
		}
	}

	_getLog(params, callback) {
		var grid = this;
		var userList = document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-user-list');
		var ajax = userList.shadowRoot.getElementById('getUserAjax');
//    var startRange = params.page * params.pageSize + 1;
//    var endRange = startRange + params.pageSize - 1;
//    var headers = {"Accept": "application/json", "Range": "items=" + startRange + "-" + endRange};
		if(ajax.etag && params.page > 0) {
			headers['If-Range'] = ajax.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				userList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic1){
					return characteristic1.name == "givenName";
				}
				function checkChar1(characteristic2){
					return characteristic2.name == "lastName";
				}
				for(var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					if(request.response[index].characteristic) {
						var givenChar = request.response[index].characteristic.find(checkChar);
						if(givenChar != undefined) {
							newRecord.givenName = givenChar.value;
						}
						var lastChar = request.response[index].characteristic.find(checkChar1);
						if(lastChar != undefined) {
							newRecord.lastName = lastChar.value;
						}
					}
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			userList.etag = null;
			var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userList.etag && params.page > 0) {
					ajax.headers['If-Range'] = userList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (userList.etag && params.page > 0) {
				ajax.headers['If-Range'] = userList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_userAddOpenModal() {
		document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-user-add').shadowRoot.getElementById('userAddModal').open();
	}
}

window.customElements.define('usekeeper-user-list', userList);
