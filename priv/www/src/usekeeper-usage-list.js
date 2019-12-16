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
import './usekeeper-icons.js';
import './style-element.js';

class usageList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="usageGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column>
					<template class="header">
						Date 
					</template>
					<template>[[item.date]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Description 
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Exclude Amount
					</template>
					<template>[[item.taxExcludedRatingAmount]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Include Amount
					</template>
					<template>[[item.taxIncludedRatingAmount]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Status
					</template>
					<template>[[item.status]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Type
					</template>
					<template>[[item.type]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax
				id="getUsageAjax"
				url="usageManagement/v4/usage"
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
		var grid = this.shadowRoot.getElementById('usageGrid');
		grid.dataProvider = this._getUsage;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.usageGrid.selectedItems = item ? [item] : [];
		} else {
			this.$.usageGrid.selectedItems = [];
		}
	}

	_getUsage(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var usageList = document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-usage-list');
		var ajax = usageList.shadowRoot.getElementById('getUsageAjax');
		if(ajax.etag && params.page > 0) {
			headers['If-Range'] = ajax.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				usageList.etag = request.xhr.getResponseHeader('ETag');
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
					if(request.response[index].date) {
						newRecord.date = request.response[index].date;
					}
					if(request.response[index].description) {
						newRecord.description = request.response[index].description;
					}
					if(request.response[index].type) {
						newRecord.type = request.response[index].type;
					}
					if(request.response[index].status) {
						newRecord.status1 = request.response[index].status;
					}
					for(var index1 in request.response[index].ratedProductUsage) {
						var rate = request.response[index].ratedProductUsage[index1];
						if(rate.taxExcludedRatingAmount) {
							newRecord.taxExcludedRatingAmount = rate.taxExcludedRatingAmount;
						}
						if(rate.taxIncludedRatingAmount) {
							newRecord.taxIncludedRatingAmount = rate.taxIncludedRatingAmount;
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
			usageList.etag = null;
			var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (usageList.etag && params.page > 0) {
					ajax.headers['If-Range'] = usageList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (usageList.etag && params.page > 0) {
				ajax.headers['If-Range'] = usageList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('usekeeper-usage-list', usageList);
