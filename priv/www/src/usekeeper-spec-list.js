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

class specList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="specGrid"
					loading="{{loading}}">
            <vaadin-grid-column>
               <template class="header">
                  Start Time
               </template>
               <template>[[item.start]]</template>
            </vaadin-grid-column>
            <vaadin-grid-column>
               <template class="header">
                  End Time
               </template>
               <template>[[item.end]]</template>
            </vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Name 
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Description 
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
            <vaadin-grid-column>
               <template class="header">
                  Type
               </template>
               <template>[[item.class]]</template>
            </vaadin-grid-column>
            <vaadin-grid-column>
               <template class="header">
                  Base
               </template>
               <template>[[item.base]]</template>
            </vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="my-icons:add"
					on-tap = "showAddSpecModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getSpecAjax"
				url="usageManagement/v4/usageSpecification"
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
			etag: {
			type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('specGrid');
		grid.dataProvider = this._getSpec;
	}

	_getSpec(params, callback) {
		var grid = this;
		var specList = document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-spec-list');
		var ajax = specList.shadowRoot.getElementById('getSpecAjax');
		if(ajax.etag && params.page > 0) {
			headers['If-Range'] = ajax.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				specList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				for(var index in request.response) {
					var newRecord = new Object();
					if(request.response[index].id) {
						newRecord.id = request.response[index].id;
					}
					if(request.response[index].name) {
						newRecord.name = request.response[index].name;
					}
					if(request.response[index].description) {
						newRecord.description = request.response[index].description;
					}
					if(request.response[index].validFor) {
						if(request.response[index].validFor.startDateTime) {
							newRecord.start = request.response[index].validFor.startDateTime;
						}
						if(request.response[index].validFor.endDateTime) {
							newRecord.end = request.response[index].validFor.endDateTime;
						}
					}
					if(request.response[index].classType) {
						newRecord.type = request.response[index].classType;
					}
					if(request.response[index].baseType) {
						newRecord.base = request.response[index].baseType;
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
			specList.etag = null;
			var toast;
			toast.text = "error";
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
				if (specList.etag && params.page > 0) {
					ajax.headers['If-Range'] = specList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (specList.etag && params.page > 0) {
				ajax.headers['If-Range'] = specList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('usekeeper-spec-list', specList);
