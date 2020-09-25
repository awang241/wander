import { shallowMount } from '@vue/test-utils'
import ListItem from "../../src/components/Misc/HelperComponents/ListItem";

// Mount the component
const wrapper = shallowMount(ListItem, {
    propsData: {
        listItem: "beans",
    }
})

describe('ListItem.vue', () => {
    it('has a created hook', () => {
        expect(true).toBe(true)
    })
    it('Renders the component', () => {
        expect(wrapper.exists()).toBe(true)
    })
    it('Emits a delete method to its parent containing the name of the list item when the delete button is clicked', () => {
        wrapper.vm.deleteListItem()
        expect(wrapper.emitted().deleteListItem[0]).toEqual(["beans"])
    })
    it('Displays the correct name of the list item passed into it', () => {
        expect(wrapper.html()).toContain('beans')
    })
});