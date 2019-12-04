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
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class specAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addSpecModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add Specification</h2></div>
			</paper-toolbar>
				<paper-input
					id="name"
					label="Name"
					value="{{spec.name}}">
				</paper-input>
				<paper-input
					id="desc"
					label="Description"
					value="{{spec.description}}">
				</paper-input>
				<div class="buttons">
					<paper-button
						raised
						class="submit-button"
						on-tap="_addSpec">
							Add
					</paper-button>
					<paper-button
						class="cancel-button"
						dialog-dismiss
						on-tap="cancelSpec">
							Cancel
					</paper-button>
					<paper-button
						toggles
						raised
						class="delete-button"
						on-tap="_deleteSpec">
							Delete
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
			id="specAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_specAddResponse"
			on-error="_specAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			spec: {
				type: Object,
			},
			charArray: {
				type: Array,
				value: function() {
					return [];
				}
			},
		}
	}

	ready() {
		super.ready()
	}

	_addSpec() {
		var ajax = this.$.specAddAjax;
		ajax.method = "POST";
		ajax.url = "/usageManagement/v4/usageSpecification";
		var spe = new Object();
		spe.name = this.$.name.value;
		spe.description = this.$.desc.value;
		ajax.body = spe;
		ajax.generateRequest();
	}

	_userAddResponse() {
		document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-spec-add').shadowRoot.getElementById('addSpecModal').close();
	}
}

window.customElements.define('usekeeper-spec-add', specAdd);
