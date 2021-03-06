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
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@polymer/paper-fab/paper-fab.js';
import './usekeeper-icons.js';
import './style-element.js';

class specificationList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="specificationGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column width="6ex" flex-grow="10">
					<template class="header">
						Name 
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="10ex" flex-grow="20">
					<template class="header">
						Description 
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="5">
					<template class="header">
						Class
					</template>
					<template>[[item.type]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex">
					<template class="header">
						Base
					</template>
					<template>[[item.base]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
						<div class="grouptitle">Validity</div>
					</template>
					<vaadin-grid-column width="12ex" flex-grow="10">
						<template class="header">
							Start
						</template>
					<template>[[item.start]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="12ex" flex-grow="10">
						<template class="header">
							End
						</template>
						<template>[[item.end]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="my-icons:add"
						on-tap="_specificationAddOpenModal">
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
			activeItem: {
				type: Boolean,
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
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.dataProvider = this._getSpec;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.specificationGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.specificationGrid.selectedItems = [];
		}
	}

	_getSpec(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var specs = document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-spec-list');
		var ajax = specs.shadowRoot.getElementById('getSpecAjax');
		if(ajax.etag && params.page > 0) {
			headers['If-Range'] = ajax.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				specs.etag = request.xhr.getResponseHeader('ETag');
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
					if(request.response[index]["@type"]) {
						newRecord.type = request.response[index]["@type"];
					}
					if(request.response[index]["@baseType"]) {
						newRecord.base = request.response[index]["@baseType"];
					}
					if(request.response[index].usageSpecCharacteristic) {
						newRecord.chars = request.response[index].usageSpecCharacteristic;
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
			specs.etag = null;
			var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (specs.etag && params.page > 0) {
					ajax.headers['If-Range'] = specs.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (specs.etag && params.page > 0) {
				ajax.headers['If-Range'] = specs.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_specificationAddOpenModal(event) {
		 document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-spec-add').shadowRoot.getElementById('specificationAddModal').open();
	}
}

window.customElements.define('usekeeper-spec-list', specificationList);
