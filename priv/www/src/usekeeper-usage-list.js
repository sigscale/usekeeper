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
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js';

class usageList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="usageGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
					<dl class="details">
						<template is="dom-if" if="{{item.date}}">
							<dt><b>Date</b></dt>
							<dd>{{item.date}}</dd>
						</template>
						<template is="dom-if" if="{{item.description}}">
							<dt><b>Description</b></dt>
							<dd>{{item.description}}</dd>
						</template>
						<template is="dom-if" if="{{item.state}}">
							<dt><b>State</b></dt>
							<dd>{{item.state}}</dd>
						</template>
						<template is="dom-if" if="{{item.type}}">
							<dt><b>Type</b></dt>
							<dd>{{item.type}}</dd>
						</template>
						<template is="dom-if" if="{{item.currencyCode}}">
							<dt><b>Currency</b></dt>
							<dd>{{item.currencyCode}}</dd>
						</template>
						<template is="dom-if" if="{{item.isTaxExempt}}">
							<dt><b>Tax Exempt</b></dt>
							<dd>{{item.isTaxExempt}}</dd>
						</template>
						<template is="dom-if" if="{{item.ratingAmountType}}">
							<dt><b>Rating Amount Type</b></dt>
							<dd>{{item.ratingAmountType}}</dd>
						</template>
						<template is="dom-if" if="{{item.taxExcludedRatingAmount}}">
							<dt><b>Tax Exclude Amount</b></dt>
							<dd>{{item.taxExcludedRatingAmount}}</dd>
						</template>
						<template is="dom-if" if="{{item.taxIncludedRatingAmount}}">
							<dt><b>Tax Include Amount</b></dt>
							<dd>{{item.taxIncludedRatingAmount}}</dd>
						</template>
						<template is="dom-if" if="{{item.taxRate}}">
							<dt><b>Tax Rate</b></dt>
							<dd>{{item.taxRate}}</dd>
						</template>
						<template is="dom-if" if="{{item.usageRatingTag}}">
							<dt><b>Rating Tag</b></dt>
							<dd>{{item.usageRatingTag}}</dd>
						</template>
						<template is="dom-if" if="{{item.id}}">
							<dt><b>Id</b></dt>
							<dd>{{item.id}}</dd>
						</template>
						<template is="dom-if" if="{{item.role}}">
							<dt><b>Role</b></dt>
							<dd>{{item.role}}</dd>
						</template>
					</dl>
					<h3 class="alarmH3">Usage Characteristics:</h3>
					<dl class="details">
						<template is="dom-if" if="{{item.usageChar}}">
							<template is="dom-repeat" items="{{item.usageChar}}" as="detail">
							   <dt>{{detail.name}}</dt>
							   <dd>{{detail.value}}</dd>
							</template>
						</template>
					</dl>
				</template>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
							id="filterDate"
							aria-label="Date"
							path="date"
							value="{{_filterDate}}">
							<input
								slot="filter"
								placeholder="Date"
								value="{{_filterDate::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						<bdo>[[item.date]]</bdo>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
							id="filterdes"
							aria-label="Description"
							path="description"
							value="{{_filterDes}}">
							<input
								slot="filter"
								placeholder="Description"
								value="{{_filterDes::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						<bdo>[[item.description]]</bdo>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
						<div class="grouptitle">Amount with Tax</div>
					</template>
					<vaadin-grid-column>
						<template class="header">
							<vaadin-grid-filter
								id="filterExAmount"
								aria-label="Excluded"
								path="taxExcludedRatingAmount"
								value="{{_filterExAmount}}">
								<input
									slot="filter"
									placeholder="Excluded"
									value="{{_filterExAmount::input}}"
									focus-target>
							</vaadin-grid-filter>
						</template>
						<template>
							<bdo>[[item.taxExcludedRatingAmount]]</bdo>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
							<vaadin-grid-filter
								id="filterRate"
								aria-label="Rate"
								path="taxRate"
								value="{{_filterRate}}">
								<input
									slot="filter"
									placeholder="Rate"
									value="{{_filterRate::input}}"
									focus-target>
							</vaadin-grid-filter>
						</template>
						<template>
							<bdo>[[item.taxRate]]</bdo>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
							<vaadin-grid-filter
								id="filterInAmount"
								aria-label="Included"
								path="taxIncludedRatingAmount"
								value="{{_filterInAmount}}">
								<input
									slot="filter"
									placeholder="Included"
									value="{{_filterInAmount::input}}"
									focus-target>
							</vaadin-grid-filter>
						</template>
						<template>
							<bdo>[[item.taxIncludedRatingAmount]]</bdo>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
							id="filterStatus"
							aria-label="Status"
							path="status"
							value="{{_filterStatus}}">
							<input
								slot="filter"
								placeholder="Status"
								value="{{_filterStatus::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						<bdo>[[item.status]]</bdo>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
							id="filterType"
							aria-label="Type"
							path="type"
							value="{{_filterType}}">
							<input
								slot="filter"
								placeholder="Type"
								value="{{_filterType::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						<bdo>[[item.type]]</bdo>
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax
				id="getUsageAjax"
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
			},
			_filterDate: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterDes: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterExAmount: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterRate: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterInAmount: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterType: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('usageGrid');
		grid.dataProvider = this._getUsage;
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.shadowRoot.getElementById('usageGrid');
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item
			}
			function checkExist(use) {
				return use.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	_getUsage(params, callback) {
		var grid = this;
		var usage = document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-usage-list');
		var ajax = usage.shadowRoot.getElementById('getUsageAjax');
		ajax.url = "usageManagement/v4/usage";
		params.filters.forEach(function(filter, index) {
			if(filter.path == "taxIncludedRatingAmount"
					|| filter.path == "taxExcludedRatingAmount"
					|| filter.path == "taxRate") {
				ajax.url += '?'+ "ratedProductUsage." + filter.path + '=' + filter.value;
			} else {
				if(index == 0) {
					if(filter.value) {
						ajax.url += '?'+ filter.path + '=' + filter.value;
					} else {
						ajax.url;
					}
				} else {
					ajax.url += '&'+ filter.path + '=' + filter.value;
				}
			}
		});
		if(!grid.size) {
			grid.size = 0;
		}
		if(usage.etag && params.page > 0) {
			headers['If-Range'] = usage.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				usage.etag = request.xhr.getResponseHeader('ETag');
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
					if(request.response[index].type) {
						newRecord.type = request.response[index].type;
					}
					if(request.response[index].status) {
						newRecord.status = request.response[index].status;
					}
					if(request.response[index].date) {
						newRecord.date = request.response[index].date;
					}
					for(var index3 in request.response[index].usageCharacteristic) {
						var useChar = request.response[index].usageCharacteristic[index3];
						newRecord.usageChar = new Array();
						var usageObject = useChar;
						for(var Name in usageObject) {
							newRecord.usageChar.push({name: Name, value: usageObject[Name]});
						}
					}
					for(var index4 in request.response[index].ratedProductUsage) {
						var rate = request.response[index].ratedProductUsage[index4];
						if(rate.currencyCode) {
							newRecord.currencyCode = rate.currencyCode;
						}
						if(rate.taxExcludedRatingAmount) {
							newRecord.taxExcludedRatingAmount = rate.taxExcludedRatingAmount;
						}
						if(rate.taxIncludedRatingAmount) {
							newRecord.taxIncludedRatingAmount = rate.taxIncludedRatingAmount;
						}
						if(rate.taxRate) {
							newRecord.taxRate = rate.taxRate;
						}
						if(rate.usageRatingTag) {
							newRecord.usageRatingTag = rate.usageRatingTag;
						}
					}
					for(var index2 in request.response[index].relatedParty) {
						var party = request.response[index].relatedParty[index2];
						if(party.id) {
							newRecord.id = party.id;
						}
						if(party.role) {
							newRecord.role = party.role;
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
			usage.etag = null;
			var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (usage.etag && params.page > 0) {
					ajax.headers['If-Range'] = usage.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (usage.etag && params.page > 0) {
				ajax.headers['If-Range'] = usage.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('usageGrid');
		grid.size = 0;
	}
}

window.customElements.define('usekeeper-usage-list', usageList);
