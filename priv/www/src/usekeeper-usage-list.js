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

class usageList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid id="usageGrid"
					loading="{{loading}}">
				<vaadin-grid-column width="6ex" flex-grow="10">
					<template class="header">
						Name 
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="10ex" flex-grow="20">
					<template class="header">
						Description 
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="5">
					<template class="header">
						Class
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex">
					<template class="header">
						Base
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="my-icons:add"
					on-tap = "showAddSpecModal">
				</paper-fab>
			</div>
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
		var grid = this.shadowRoot.getElementById('usageGrid');
		grid.dataProvider = this._getUsage;
	}

	_getUsage(params) {
	}
}

window.customElements.define('usekeeper-usage-list', usageList);
